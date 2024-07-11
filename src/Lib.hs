module Lib (runProgram) where

import Control.Monad.Except (ExceptT, throwError, runExceptT)
import Control.Monad.Reader (ReaderT, ask, asks, local, runReaderT)
import Control.Monad.Trans (lift)
import qualified Environment as E
import Name (FreshNameT, Name, fresh, runFreshNameT)
import Term (Term (..), alphaEquivalence, Difference, Program, Statement (..))
import Data.Foldable (foldlM)
import Text.Printf (printf)
import Control.Monad.IO.Class (liftIO)

data Error
  = UndefinedVariable Name
  | CannotSynthetizeTypeFor Term
  | TypeError VType Term
  | TypeMismatch Term Term Difference
  | ReadBackTypeError VType Value
  | CannotApplyTerm Term VType
  | TermApplicationError Value
  | Todo
  deriving Show

type Environment = E.Environment Name Value

type Context = E.Environment Name Bound

data Closure = Closure Environment Name Term

type VType = Value

data Value
  = VPi VType Closure
  | VLambda Closure
  | VUniverse
  | -- We need to keep track of the type of neutral term
    VNeutral VType Neutral

instance Show Value where
  show (VPi argType (Closure _ argName body)) =
    printf "Π (%s : %s) -> %s" (show argName) (show argType) (show body)
  show (VLambda (Closure _ argName body)) =
    printf "λ %s. %s" (show argName) (show body)
  show VUniverse = "𝒰"
  show (VNeutral typ term) = printf "%s : %s" (show term) (show typ)

-- Neutral expressions are expression that are temporarly stuck
data Neutral
  = -- A function application is stuck if we don't
    -- know the function that we are applying, the
    -- argument must be in normal form, meaning we
    -- can't reduce it further
    NApplication Neutral Normal
  | -- A variable that is nout bound in this context
    NVariable Name

instance Show Neutral where
  show (NApplication fun arg) = printf "(%s %s)" (show fun) (show arg)
  show (NVariable name) = show name

data Normal
  = -- Normal forms are values annotated with a type
    NAnnotated VType Value

instance Show Normal where
  show (NAnnotated typ term) = printf "%s : %s" (show term) (show typ)

data Bound
  = Definition VType Value
  | Bind VType
  deriving Show

type EvaluationT m = (ExceptT Error (FreshNameT m))
type ContextualEvaluationT e m = ReaderT e (EvaluationT m)

applyClosure :: (Monad m) => Closure -> Value -> ContextualEvaluationT e m Value
applyClosure (Closure env argName body) arg = do
  let env' = E.extend argName arg env
  lift $ runReaderT (eval body) env'

apply :: (Monad m) => Value -> Value -> ContextualEvaluationT e m Value
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

eval :: (Monad m) => Term -> ContextualEvaluationT Environment m Value
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
readbackNormal :: (Monad m) => Normal -> ContextualEvaluationT Context m Term
readbackNormal (NAnnotated (VPi argType retTyConstructor) f) = do
  -- Constuct the neutral version of the argument
  argName <- fresh
  let neutralArg = VNeutral argType $ NVariable argName
  -- Apply the argument to the type constructor and to the function to obtain
  -- the return value and its type
  retType <- applyClosure retTyConstructor neutralArg
  ret <- apply f neutralArg
  -- Now we can readback the return to read the body of the lambda
  let bindArgument = E.extend argName $ Bind argType
  body <- local bindArgument $ readbackNormal $ NAnnotated retType ret
  -- Construct the lambda
  pure $ Lambda argName body
readbackNormal (NAnnotated VUniverse (VPi argType closure)) = do
  -- Constuct the neutral value of the argument
  argName <- fresh
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

readbackNeutral :: (Monad m) => Neutral -> ContextualEvaluationT Context m Term
readbackNeutral (NVariable name) = pure $ Variable name
readbackNeutral (NApplication fun arg) = do
  fun <- readbackNeutral fun
  arg <- readbackNormal arg
  pure $ Application fun arg

asEnvironment :: Context -> Environment
asEnvironment = E.on convert
  where
    -- We don't need the type of a value during evaluation
    convert (name, Definition _ value) = (name, value)
    -- If it's a type binding we transform it in a stuck variable, since 
    -- obviusly we dont' have a value
    convert (name, Bind typ) = (name, VNeutral typ (NVariable name))

evalInContext :: (Monad m) => Term -> ContextualEvaluationT Context m Value
evalInContext t = do
  ctx <- ask
  lift $ runReaderT (eval t) $ asEnvironment ctx

-- Synthetize a type (In Term form since it's being elaborated) for `Term` and 
-- elaborate it
synthetize :: (Monad m) => Term -> ContextualEvaluationT Context m (Term, Term)
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
check :: (Monad m) => Term -> VType -> ContextualEvaluationT Context m Term
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


equivalence :: (Monad m) => VType -> Value -> Value -> ContextualEvaluationT Context m ()
equivalence typ a b = do
  -- Bring the values in notmal form
  a <- readbackNormal $ NAnnotated typ a
  b <- readbackNormal $ NAnnotated typ b
  case alphaEquivalence a b of
    Right () -> pure ()
    Left difference -> throwError $ TypeMismatch a b difference

-- evalProgram :: [Statement] -> ProgramM (Environment Value)
-- evalProgram = foldlM step E.empty
--   where
--     step env (Define name term) = do
--       term <- runReaderT (eval term) env
--       pure $ E.extend name term env
--     step env (Display term) = do
--       term <- runReaderT (normalize term) env
--       liftIO $ print term
--       pure env
--



runProgramM :: Program -> EvaluationT IO Context
runProgramM = foldlM step E.empty
  where
    step :: Context -> Statement -> EvaluationT IO Context
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

runProgram :: Program -> IO ()
runProgram program = do
  result <- runFreshNameT $ runExceptT $ runProgramM program
  case result of
    Left error -> putStrLn $ printf "ERROR: %s" (show error)
    Right _ -> pure ()

-- >>> import Name
-- >>> let x = mkName "x"
-- >>> let id = mkName "id"
-- >>> let t = mkName "T"
-- >>> let idk = mkName "_"
-- >>> let definition = (Lambda idk (Lambda x (Variable x)))
-- >>> let typ = Pi t Universe (Pi idk (Variable t) (Variable t))
-- >>> runProgram [Define id (Annotation definition typ), Display (Variable id)]

-- >>> import Name
-- >>> let x = mkName "x"
-- >>> let y = mkName "y"
-- >>> let id = mkName "id"
-- >>> let app = mkName "app"
-- >>> let t = mkName "T"
-- >>> let appDef = (Lambda x (Lambda y (Application (Variable x) (Variable y))))
-- >>> let idDef = Annotation (Lambda x (Variable x)) (Pi t Universe (Variable t))
-- >>> runProgram [Define id idDef, Define app appDef, Display (Application (Variable app) (Variable id))]
-- getLinkDeps: Home module not loaded Environment main-7311c3dc8d7e318622b94ef5b8410fb2ef9e136d

-- >>> import Name
-- >>> let x = mkName "x"
-- >>> let s = Lambda x (Application (Variable x) (Variable x))
-- >>> let q = Application s s
-- >>> nooo runProgram [Display q]
