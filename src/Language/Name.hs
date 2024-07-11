{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Name
  ( Name,
    MonadFreshName,
    FreshNameT,
    runFreshNameT,
    fresh,
    mkName,
    freshHint,
  )
where

import Control.Monad.Except (ExceptT, MonadError, catchError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT, evalStateT, get, modify)
import Control.Monad.Trans (MonadTrans, lift)
import Text.Printf (printf)

data Name
  = UserDefined String
  | MaachineGenerated Int
  | MaachineGeneratedHinted Name Int
  deriving (Eq)

mkName :: String -> Name
mkName = UserDefined

instance Show Name where
  show (UserDefined n) = n
  show (MaachineGenerated id) = printf "mg$%d" id
  show (MaachineGeneratedHinted hint id) = printf "mg(%s)$%d" (show hint) id

class (Monad m) => MonadFreshName m where
  fresh :: m Name
  freshHint :: Name -> m Name

newtype FreshNameT m a = FreshNameT {unFreshNameT :: StateT Int m a}
  deriving (MonadTrans, Monad, Applicative, Functor)

freshId :: (Monad m) => FreshNameT m Int
freshId = FreshNameT $ do
  id <- get
  modify (+ 1)
  pure id

instance (Monad m) => MonadFreshName (FreshNameT m) where
  fresh = MaachineGenerated <$> freshId

  freshHint hint = MaachineGeneratedHinted hint <$> freshId

runFreshNameT :: (Monad m) => FreshNameT m a -> m a
runFreshNameT = flip evalStateT 0 . unFreshNameT

-- MTL instances
instance (MonadIO m) => MonadIO (FreshNameT m) where
  liftIO = lift . liftIO

-- liftCatch :: Catch e m (a,s) -> Catch e (StateT s m) a
-- liftCatch catchE m h =
--     StateT $ \ s -> runStateT m s `catchE` \ e -> runStateT (h e) s

instance (MonadError e m) => MonadError e (FreshNameT m) where
  throwError = lift . throwError

  catchError m h = FreshNameT $ catchError (unFreshNameT m) (unFreshNameT <$> h)

instance (MonadFreshName m) => MonadFreshName (ExceptT e m) where
  fresh = lift fresh
  freshHint = lift . freshHint

instance (MonadFreshName m) => MonadFreshName (ReaderT e m) where
  fresh = lift fresh
  freshHint = lift . freshHint
