{-# LANGUAGE FlexibleContexts #-}

module Language.Core.Engine (runProgram, Value) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask, asks, local, runReaderT)
import Data.Foldable (foldlM)
import Language.Core.Name (MonadName, Name, freshHint)
import Language.Core.Syntax
  ( Difference,
    Program,
    Statement (..),
    Term (..),
    alphaEquivalence,
  )
import Language.Core.Value
  ( Bound (..),
    Closure (..),
    Context,
    Environment,
    Neutral (..),
    Normal (..),
    VType,
    Value (..),
  )
import qualified Language.Core.Environment as E
import Text.Printf (printf)

data Error
  = UndefinedVariable Name
  | CannotSynthetizeTypeFor Term
  | TypeError VType Term
  | TypeMismatch Term Term Difference
  | ReadBackTypeError VType Value
  | CannotApplyTerm Term VType
  | TermApplicationError Value
  deriving (Show)

applyClosure :: (MonadError Error m) => Closure -> Value -> m Value
applyClosure (Closure env argName body) arg = do
  let env' = E.extend argName arg env
  runReaderT (eval body) env'

argName :: Closure -> Name
argName (Closure _ argName _) = argName

apply :: (MonadError Error m) => Value -> Value -> m Value
-- If it is a lambda-closure we simply apply the body in the closure context
apply (VLambda closure) arg = applyClosure closure arg
-- If it is a pi-closure that is neutral we canno't evaluate the function
-- since clearly we don't know hot to apply a neutral function but we can
-- evaluate further the expression since we can apply it's type
apply (VNeutral (VPi argType retTyConstructor) neutralFun) arg = do
  -- Evaluate the type by applying the argument, hence we can know the type
  -- of applying the argument to the function
  resultType <- applyClosure retTyConstructor arg
  -- Annotate the arguments with it's type that we got from Pi
  let arg' = NAnnotated arg argType
  -- The application it's still stuck but we can reconstruct it
  let neutralApplication = NApplication neutralFun arg'
  pure $ VNeutral resultType neutralApplication
apply fun _ = throwError $ TermApplicationError fun

eval :: (MonadReader Environment m, MonadError Error m) => Term -> m Value
eval (Annotation term _) = eval term
eval Universe = pure VUniverse
eval (Pi param typ body) = do
  -- Evaluate the type
  typ <- eval typ
  -- Construct the Pi-closure
  env <- ask
  pure $ VPi typ $ Closure env param body
eval (Lambda param body) = do
  env <- ask
  pure $ VLambda $ Closure env param body
eval (Application fun arg) = do
  -- Evaluate the function and the argument
  fun <- eval fun
  arg <- eval arg
  apply fun arg
eval (Variable name) = do
  mval <- asks $ E.lookup name
  case mval of
    Just val -> pure val
    _ -> throwError $ UndefinedVariable name

-- Reading back transform a value in it's normal form
readbackNormal ::
  (MonadReader Context m, MonadError Error m, MonadName m) =>
  Normal ->
  m Term
readbackNormal (NAnnotated (VPi argType closure) f) = do
  -- Constuct the neutral version of the argument
  argName <- freshHint $ argName closure
  let neutralArg = VNeutral argType $ NVariable argName
  -- Apply the argument to the type constructor and to the function to obtain
  -- the return value and its type
  retType <- applyClosure closure neutralArg
  ret <- apply f neutralArg
  -- Now we can readback the return to read the body of the lambda
  let bindArgument = E.extend argName $ Bind argType
  body <- local bindArgument $ readbackNormal $ NAnnotated retType ret
  -- Construct the lambda
  pure $ Lambda argName body
readbackNormal (NAnnotated VUniverse (VPi argType closure)) = do
  -- Constuct the neutral value of the argument
  argName <- freshHint $ argName closure
  let argValue = VNeutral argType $ NVariable argName
  -- Apply the argument to the closure, since it's the result of a Pi, it's a
  -- type so it has type Universe so we can annotate it directly.
  -- Now we can read back the value to get the n.f. body
  ret <- applyClosure closure argValue
  let ret' = NAnnotated VUniverse ret
  let bindArg = E.extend argName $ Bind argType
  body <- local bindArg $ readbackNormal ret'
  -- We can readback directly the type of the argument
  argType <- readbackNormal $ NAnnotated VUniverse argType
  -- Contruct the Pi type
  pure $ Pi argName argType body
readbackNormal (NAnnotated VUniverse VUniverse) = pure Universe
readbackNormal (NAnnotated _ (VNeutral _ n)) = readbackNeutral n
readbackNormal (NAnnotated typ value) = throwError $ ReadBackTypeError typ value

readbackNeutral ::
  (MonadReader Context m, MonadError Error m, MonadName m) =>
  Neutral ->
  m Term
readbackNeutral (NVariable name) = pure $ Variable name
readbackNeutral (NApplication fun arg) = do
  fun <- readbackNeutral fun
  arg <- readbackNormal arg
  pure $ Application fun arg

asEnvironment :: Context -> Environment
asEnvironment = E.mapWithKey convert
  where
    -- We don't need the type of a value during evaluation
    convert _ (Definition _ value) = value
    -- If it's a type binding we transform it in a stuck variable, since
    -- obviusly we dont' have a value
    convert name (Bind typ) = VNeutral typ (NVariable name)

evalInContext :: (MonadReader Context m, MonadError Error m) => Term -> m Value
evalInContext t = do
  env <- asks asEnvironment
  runReaderT (eval t) env

-- Synthetize a type (In Term form since it's being elaborated) for `Term` and
-- elaborate it
synthetize ::
  (MonadError Error m, MonadReader Context m, MonadName m) =>
  Term ->
  m (Term, Term)
synthetize (Annotation term typ) = do
  typ <- check typ VUniverse
  typ' <- evalInContext typ
  term <- check term typ'
  pure (typ, term)
synthetize Universe = pure (Universe, Universe)
synthetize (Pi argName argType body) = do
  argType <- check argType VUniverse
  -- We still need the term form for the elavorated term
  argType' <- evalInContext argType
  let bindArg = E.extend argName (Bind argType')
  body <- local bindArg $ check body VUniverse
  pure (Universe, Pi argName argType body)
synthetize (Application fun arg) = do
  -- Obtain the type of the function
  (funTy, fun) <- synthetize fun
  funTy <- evalInContext funTy
  case funTy of
    -- The type of function is Pi
    VPi argType closure -> do
      -- Check that the type of the argument and the expected one are the same
      arg <- check arg argType
      arg' <- evalInContext arg
      -- Compute the return type since it's dependent on the argument
      retType <- applyClosure closure arg'
      retType <- readbackNormal (NAnnotated VUniverse retType)
      return (retType, Application fun arg)
    _ -> throwError $ CannotApplyTerm fun funTy
synthetize (Variable name) = do
  -- Lookup the type of the variable in the context
  mtyp <- asks $ E.lookup name
  typ <- case mtyp of
    Just (Bind typ) -> pure typ
    Just (Definition typ _) -> pure typ
    Nothing -> throwError $ UndefinedVariable name
  typ <- readbackNormal (NAnnotated VUniverse typ)
  pure (typ, Variable name)
synthetize t = throwError $ CannotSynthetizeTypeFor t

-- Check if `Term` has type `Type` returning the elaborate version
check ::
  (MonadReader Context m, MonadError Error m, MonadName m) =>
  Term ->
  VType ->
  m Term
check (Lambda argName body) (VPi argType closure) = do
  -- (λ argName . body)     (Π (_ : argType) closure)
  -- Neutral function argument and compute the return type
  let arg = VNeutral argType $ NVariable argName
  retType <- applyClosure closure arg
  -- Check that the body with the arg in context has is of the return type
  let bindArg = E.extend argName $ Bind argType
  body <- local bindArg $ check body retType
  pure $ Lambda argName body
check term@(Lambda _ _) typ = throwError $ TypeError typ term
check term typ = do
  -- Trt to synthetize a type for term
  (synthetizedType, term) <- synthetize term
  synthetizedType <- evalInContext synthetizedType
  -- Check that the provided type is the same as the synthetized one
  equivalence VUniverse typ synthetizedType
  pure term

equivalence ::
  (MonadReader Context m, MonadError Error m, MonadName m) =>
  VType ->
  Value ->
  Value ->
  m ()
equivalence typ a b = do
  -- Bring the values in notmal form
  a <- readbackNormal $ NAnnotated typ a
  b <- readbackNormal $ NAnnotated typ b
  case alphaEquivalence a b of
    Right () -> pure ()
    Left difference -> throwError $ TypeMismatch a b difference

runProgramM :: (MonadName m, MonadError Error m, MonadIO m) => Program -> m Context
runProgramM = foldlM step E.empty
  where
    step ctx (Define name term) = do
      (typ, term) <- runReaderT (synthetize term) ctx
      let env = asEnvironment ctx
      typ <- runReaderT (eval typ) env
      term <- runReaderT (eval term) env
      pure $ E.extend name (Definition typ term) ctx
    step ctx (Display term) = do
      (typ, term) <- runReaderT (synthetize term) ctx
      let env = asEnvironment ctx
      typ <- runReaderT (eval typ) env
      term <- runReaderT (eval term) env
      liftIO $ putStrLn $ printf "%s : %s" (show term) (show typ)
      pure ctx

runProgram :: (MonadName m, MonadIO m) => Program -> m ()
runProgram program = do
  result <- runExceptT $ runProgramM program
  case result of
    Left error -> liftIO $ putStrLn $ printf "ERROR: %s" (show error)
    Right _ -> pure ()
