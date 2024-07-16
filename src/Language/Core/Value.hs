{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Core.Value
  ( Value (..),
    VType,
    Closure (..),
    Normal (..),
    Neutral (..),
    MonadMetaContext (..),
    MonadEnvironment (..),
    MonadContext (..),
    Bound (..),
    Context,
    insert,
    runEnvironmentT,
    runContextT,
    emptyEnv,
    asEnvironment,
    runMetaContextT,
    argName,
    names,
    Env (..),
    MValue (..)
  )
where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT, MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader
  ( MonadReader (local),
    ReaderT (runReaderT),
    ask,
    asks,
    mapReaderT,
  )
import Control.Monad.State (StateT, evalStateT, gets, modify, get)
import Control.Monad.Trans (MonadTrans, lift)
import qualified Data.Map as M
import Language.Core.Name (MName, MonadName (..), NameT, VName (..), mapNameT)
import Language.Core.Syntax (Term)
import Prelude hiding (lookup)

type VType = Value

data Value
  = VPi VType Closure
  | VLambda Closure
  | VUniverse
  | VMeta MName
  | -- We need to keep track of the type of neutral term
    VNeutral VType Neutral

data Closure = Closure Environment VName Term

argName :: Closure -> VName
argName (Closure _ argName _) = argName

data Normal
  = -- Normal forms are values annotated with a type
    NAnnotated VType Value

-- Neutral expressions are expression that are temporarly stuck
data Neutral
  = -- A function application is stuck if we don't
    -- know the function that we are applying, the
    -- argument must be in normal form, meaning we
    -- can't reduce it further
    NApplication Neutral Normal
  | -- A variable that is nout bound in this context
    NVariable VName

data Bound
  = Definition VType Value
  | Bind VType

newtype Env k v = Env {unEnv :: M.Map k v}
  deriving Show

names :: Env k v -> [k]
names = M.keys . unEnv

insert :: (Ord k) => k -> v -> Env k v -> Env k v
insert k v = Env . M.insert k v . unEnv

emptyEnv :: Env k v
emptyEnv = Env M.empty

type Environment = Env VName Value

asEnvironment :: Context -> Environment
asEnvironment = Env . M.mapWithKey convert . unEnv
  where
    -- We don't need the type of a value during evaluation
    convert _ (Definition _ value) = value
    -- If it's a type binding we transform it in a stuck variable, since
    -- obviusly we dont' have a value
    convert name (Bind typ) = VNeutral typ (NVariable name)

class (Monad m) => MonadEnvironment m where
  lookupValue :: VName -> m (Maybe Value)
  environment :: m Environment

newtype EnvironmentT m a = EnvironmentT
  { unEvnironmentT :: ReaderT Environment m a
  }
  deriving (MonadTrans, Monad, Applicative, Functor)

runEnvironmentT :: Environment -> EnvironmentT m a -> m a
runEnvironmentT env = flip runReaderT env . unEvnironmentT

instance (Monad m) => MonadEnvironment (EnvironmentT m) where
  lookupValue name = EnvironmentT $ asks $ M.lookup name . unEnv
  environment = EnvironmentT ask

type Context = Env VName Bound

class (Monad m) => MonadContext m where
  lookupBind :: VName -> m (Maybe Bound)

  context :: m Context

  bindLocally :: VName -> Bound -> m a -> m a

newtype ContextT m a = ContextT
  { unContextT :: ReaderT Context m a
  }
  deriving (MonadTrans, Monad, Applicative, Functor)

runContextT :: Context -> ContextT m a -> m a
runContextT env = flip runReaderT env . unContextT

instance (Monad m) => MonadContext (ContextT m) where
  lookupBind name = ContextT $ asks $ M.lookup name . unEnv

  bindLocally name value ma =
    ContextT $ local (insert name value) $ unContextT ma

  context = ContextT ask

data MValue
  = Solved Value
  | Unsolved
  

type MetaContext = Env MName MValue

class (Monad m) => MonadMetaContext m where
  lookupMeta :: MName -> m Value
  solveMeta :: MName -> Value -> m ()
  metaContext :: m MetaContext

newtype MetaContextT m a = MetaContextT
  { unMetaContextT :: StateT MetaContext m a
  }
  deriving (MonadTrans, Monad, Applicative, Functor)

runMetaContextT :: (Monad m) =>
  MetaContext ->
  MetaContextT m a ->
  m a
runMetaContextT ctx = flip evalStateT ctx . unMetaContextT

instance (Monad m) => MonadMetaContext (MetaContextT m) where
  lookupMeta name = MetaContextT $ do
    mval <- gets $ M.lookup name . unEnv
    case mval of
      Just (Solved val) -> pure val
      Just Unsolved -> pure $ VMeta name
      Nothing -> do
        -- We haven't already encountered this meta variable but it's unsolved
        modify $ insert name Unsolved
        pure $ VMeta name

  metaContext = MetaContextT get

  solveMeta name value =
    MetaContextT $ modify $ insert name (Solved value)

-- MTL instances
instance (MonadName m) => MonadName (MetaContextT m) where
  freshVName = lift freshVName
  refreshVName = lift . refreshVName
  mkVName = lift . mkVName
  mkVDiscarded = lift mkVDiscarded

  mkMName = lift mkMName

instance (MonadName m) => MonadName (ContextT m) where
  freshVName = lift freshVName
  refreshVName = lift . refreshVName
  mkVName = lift . mkVName
  mkVDiscarded = lift mkVDiscarded

  mkMName = lift mkMName

instance (MonadMetaContext m) => MonadMetaContext (EnvironmentT m) where
  lookupMeta = lift . lookupMeta
  solveMeta n v = lift $ solveMeta n v
  metaContext = lift metaContext

instance (MonadMetaContext m) => MonadMetaContext (ContextT m) where
  lookupMeta = lift . lookupMeta
  solveMeta n v = lift $ solveMeta n v
  metaContext = lift metaContext

instance (MonadError e m) => MonadError e (EnvironmentT m) where
  throwError = lift . throwError

  catchError m h =
    EnvironmentT $ catchError (unEvnironmentT m) (unEvnironmentT <$> h)

instance (MonadError e m) => MonadError e (ContextT m) where
  throwError = lift . throwError

  catchError m h =
    ContextT $ catchError (unContextT m) (unContextT <$> h)

instance (MonadMetaContext m) => MonadMetaContext (ExceptT e m) where
  lookupMeta = lift . lookupMeta
  solveMeta n v = lift $ solveMeta n v
  metaContext = lift metaContext

instance (MonadIO m) => MonadIO (MetaContextT m) where
  liftIO = lift . liftIO

instance (MonadContext m) => MonadContext (ReaderT a m) where
  lookupBind = lift . lookupBind
  context = lift context
  bindLocally name val = mapReaderT (bindLocally name val)

instance (MonadMetaContext m) => MonadMetaContext (ReaderT a m) where
  lookupMeta = lift . lookupMeta
  solveMeta name = lift . solveMeta name
  metaContext = lift metaContext

instance (MonadContext m) => MonadContext (NameT m) where
  lookupBind = lift . lookupBind
  context = lift context
  bindLocally name val = mapNameT (bindLocally name val)

instance (MonadMetaContext m) => MonadMetaContext (NameT m) where
  lookupMeta = lift . lookupMeta
  solveMeta name = lift . solveMeta name
  metaContext = lift metaContext
