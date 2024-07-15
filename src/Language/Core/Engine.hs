{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Core.Engine (runProgram) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks, local, runReaderT)
import Data.Foldable (foldlM)
import qualified Data.Map as M
import Language.Core.Name (
  MName, MonadName, VName (..), mkMName, refreshVName, runNameT)
import Language.Core.Syntax (Program, Statement (..), Term (..))
import Language.Core.Value
  ( Bound (..),
    Closure (..),
    Context,
    MonadContext (..),
    MonadEnvironment (..),
    MonadMetaContext (..),
    Neutral (..),
    Normal (..),
    VType,
    Value (..),
    argName,
    asEnvironment,
    emptyEnv,
    insert,
    runContextT,
    runEnvironmentT,
    runMetaContextT,
  )
import Control.Arrow ((***))
import Text.Printf (printf)

data Error
  = UndefinedVariable VName
  | CannotSynthetizeTypeFor Term
  | TypeError VType Term
  | TypeMismatch Term Term
  | ReadBackTypeError VType Value
  | CannotApplyTerm Term VType
  | TermApplicationError Value
  | Todo String
  deriving (Show)

type MonadMeta m = (MonadMetaContext m, MonadError Error m)

type MonadEval m = (MonadMeta m, MonadEnvironment m)

type MonadType m = (MonadMeta m, MonadContext m, MonadName m)

applyClosure :: (MonadMeta m) => Closure -> Value -> m Value
applyClosure (Closure env argName body) arg =
  runEnvironmentT (insert argName arg env) $ eval body

apply :: (MonadMeta m) => Value -> Value -> m Value
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

eval :: (MonadEval m) => Term -> m Value
eval (Annotation term _) = eval term
eval Universe = pure VUniverse
eval (Pi param typ body) = do
  -- Evaluate the type
  typ <- eval typ
  -- Construct the Pi-closure
  env <- environment
  pure $ VPi typ $ Closure env param body
eval (Lambda param body) = do
  env <- environment
  pure $ VLambda $ Closure env param body
eval (Application fun arg) = do
  -- Evaluate the function and the argument
  fun <- eval fun
  arg <- eval arg
  apply fun arg
eval (Variable name) = do
  mval <- lookupValue name
  case mval of
    Just val -> pure val
    _ -> throwError $ UndefinedVariable name
eval (Meta name) = lookupMeta name

-- Reading back transform a value in it's normal form
readbackNormal :: (MonadType m) => Normal -> m Term
readbackNormal (NAnnotated (VPi argType closure) f) = do
  -- Constuct the neutral version of the argument
  argName <- refreshVName $ argName closure
  let neutralArg = VNeutral argType $ NVariable argName
  -- Apply the argument to the type constructor and to the function to obtain
  -- the return value and its type
  retType <- applyClosure closure neutralArg
  ret <- apply f neutralArg
  -- Now we can readback the return to read the body of the lambda
  body <- bindLocally argName (Bind argType) $ readbackNormal $ NAnnotated retType ret
  -- Construct the lambda
  pure $ Lambda argName body
readbackNormal (NAnnotated VUniverse (VPi argType closure)) = do
  -- Constuct the neutral value of the argument
  argName <- refreshVName $ argName closure
  let argValue = VNeutral argType $ NVariable argName
  -- Apply the argument to the closure, since it's the result of a Pi, it's a
  -- type so it has type Universe so we can annotate it directly.
  -- Now we can read back the value to get the n.f. body
  ret <- applyClosure closure argValue
  let ret' = NAnnotated VUniverse ret
  body <- bindLocally argName (Bind argType) $ readbackNormal ret'
  -- We can readback directly the type of the argument
  argType <- readbackNormal $ NAnnotated VUniverse argType
  -- Contruct the Pi type
  pure $ Pi argName argType body
readbackNormal (NAnnotated VUniverse VUniverse) = pure Universe
readbackNormal (NAnnotated _ (VNeutral _ n)) = readbackNeutral n
readbackNormal (NAnnotated _ (VMeta name)) = pure $ Meta name
readbackNormal (NAnnotated typ value) = throwError $ ReadBackTypeError typ value

readbackNeutral :: (MonadType m) => Neutral -> m Term
readbackNeutral (NVariable name) = pure $ Variable name
readbackNeutral (NApplication fun arg) = do
  fun <- readbackNeutral fun
  arg <- readbackNormal arg
  pure $ Application fun arg

evalInContext :: (MonadType m) => Term -> m Value
evalInContext t = do
  env <- asEnvironment <$> context
  runEnvironmentT env $ eval t

-- Synthetize a type (In Term form since it's being elaborated) for `Term` and
-- elaborate it
synthetize :: (MonadType m) => Term -> m (Term, Term)
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
  body <- bindLocally argName (Bind argType') $ check body VUniverse
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
  mtyp <- lookupBind name
  typ <- case mtyp of
    Just (Bind typ) -> pure typ
    Just (Definition typ _) -> pure typ
    Nothing -> throwError $ UndefinedVariable name
  typ <- readbackNormal (NAnnotated VUniverse typ)
  pure (typ, Variable name)
synthetize t = throwError $ CannotSynthetizeTypeFor t

-- Check if `Term` has type `Type` returning the elaborate version
check :: (MonadType m) => Term -> VType -> m Term
check (Lambda argName body) (VPi argType closure) = do
  -- (λ argName . body)     (Π (_ : argType) closure)
  -- Neutral function argument and compute the return type
  let arg = VNeutral argType $ NVariable argName
  retType <- applyClosure closure arg
  -- Check that the body with the arg in context has is of the return type
  body <- bindLocally argName (Bind argType) $ check body retType
  pure $ Lambda argName body
check term@(Lambda _ _) typ = throwError $ TypeError typ term
check (Meta _) _ = Meta <$> mkMName
check term typ = do
  -- Trt to synthetize a type for term
  (synthetizedType, term) <- synthetize term
  synthetizedType <- evalInContext synthetizedType
  -- Check that the provided type is the same as the synthetized one
  equivalence VUniverse typ synthetizedType
  pure term

equivalence :: (MonadType m) => VType -> Value -> Value -> m ()
equivalence typ a b = do
  -- Bring the values in notmal form
  a <- readbackNormal $ NAnnotated typ a
  b <- readbackNormal $ NAnnotated typ b
  alphaEquivalence a b


type NameAssoc = M.Map VName MName

type MonadAlpha m =
  ( MonadReader (NameAssoc, NameAssoc) m,
    MonadContext m,
    MonadMeta m,
    MonadName m
  )

-- Names in values are not used as actual names but they are just used as unique
-- identifiers, so we don't really care about having a fresh NameT for computing
-- α-equivalence
alphaEquivalence :: (MonadContext m, MonadMeta m) => Term -> Term -> m ()
alphaEquivalence lhs rhs = runNameT $ runReaderT (alphaEquivalenceM lhs rhs) (M.empty, M.empty)

alphaEquivalenceM :: (MonadAlpha m) => Term -> Term -> m ()
alphaEquivalenceM l@(Variable nameL) r@(Variable nameR) = do
  mlhs <- asks $ M.lookup nameL . fst
  mrhs <- asks $ M.lookup nameR . snd
  case (mlhs, mrhs) of
    (Nothing, Nothing) | nameL == nameR -> pure ()
    (Just lhs, Just rhs) | lhs == rhs -> pure ()
    _ -> throwError $ TypeMismatch l r
alphaEquivalenceM (Lambda lArg lBody) (Lambda rArg rBody) = do
  id <- mkMName
  let extend = M.insert lArg id *** M.insert rArg id
  local extend $ alphaEquivalenceM lBody rBody
alphaEquivalenceM (Pi lArg lType lBody) (Pi rArg rType rBody) = do
  alphaEquivalenceM lType rType
  id <- mkMName
  let extend = M.insert lArg id *** M.insert rArg id
  local extend $ alphaEquivalenceM lBody rBody
alphaEquivalenceM (Application lFun lArg) (Application rFun rArg) = do
  alphaEquivalenceM lFun rFun
  alphaEquivalenceM lArg rArg
alphaEquivalenceM (Annotation lTerm lType) (Annotation rTerm rType) = do
  alphaEquivalenceM lTerm rTerm
  alphaEquivalenceM lType rType
alphaEquivalenceM Universe Universe = pure ()
alphaEquivalenceM (Meta mname) target = do
  throwError $ Todo $ "Unimplemented meta inference ~ " ++ show target
alphaEquivalenceM target (Meta mname) = do
  throwError $ Todo $ "Unimplemented meta inference ~ " ++ show target
alphaEquivalenceM lhs rhs = throwError $ TypeMismatch lhs rhs

runProgramM :: (MonadName m, MonadMeta m, MonadIO m) => Program -> m Context
runProgramM = foldlM step emptyEnv
  where
    step ctx (Define name term) = do
      (typ, term) <- runContextT ctx $ synthetize term
      let env = asEnvironment ctx
      typ <- runEnvironmentT env $ eval typ
      term <- runEnvironmentT env $ eval term
      pure $ insert name (Definition typ term) ctx
    step ctx (Display term) = do
      (typ, term) <- runContextT ctx $ synthetize term
      let env = asEnvironment ctx
      typ <- runEnvironmentT env $ eval typ
      term <- runEnvironmentT env $ eval term
      liftIO $ putStrLn $ printf "%s : %s" (show term) (show typ)
      pure ctx

runProgram :: (MonadName m, MonadIO m) => Program -> m ()
runProgram program = do
  result <- runMetaContextT emptyEnv $ runExceptT $ runProgramM program
  case result of
    Left error -> liftIO $ putStrLn $ printf "ERROR: %s" (show error)
    Right _ -> pure ()
