{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Core.Name
  ( VName (..),
    MName,
    MonadName (..),
    NameT,
    runNameT,
    mapNameT,
  )
where

import Control.Monad.Except (ExceptT, MonadError, catchError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State
  ( MonadState,
    StateT (..),
    evalStateT,
    get,
    modify,
    put,
    runStateT,
  )
import Control.Monad.Trans (MonadTrans, lift)
import Text.Printf (printf)

data VName
  = MachineName Int
  | AnnotatedName String Int
  | Discarded Int
  deriving (Eq, Ord)

newtype MName
  = MName Int
  deriving (Eq, Ord)

instance Show VName where
  show (MachineName id) = printf "$%d" id
  show (AnnotatedName ann id) = printf "%s$%d" ann id
  show (Discarded id) = printf "discarded$%d" id

instance Show MName where
  show (MName id) = printf "?%d" id

class (Monad m) => MonadName m where
  freshVName :: m VName
  refreshVName :: VName -> m VName
  mkVName :: String -> m VName
  mkVDiscarded :: m VName

  mkMName :: m MName

newtype NameT m a = NameT {unNameT :: StateT Int m a}
  deriving (MonadTrans, Monad, Applicative, Functor)

freshId :: (Monad m) => NameT m Int
freshId = NameT $ do
  id <- get
  modify (+ 1)
  pure id

instance (Monad m) => MonadName (NameT m) where
  freshVName = MachineName <$> freshId

  refreshVName (MachineName _) = MachineName <$> freshId
  refreshVName (AnnotatedName ann _) = AnnotatedName ann <$> freshId
  refreshVName (Discarded _) = Discarded <$> freshId

  mkVName ann = AnnotatedName ann <$> freshId
  mkVDiscarded = Discarded <$> freshId

  mkMName = MName <$> freshId

runNameT :: (Monad m) => NameT m a -> m a
runNameT = flip evalStateT 0 . unNameT

mapNameT :: (Monad m) => (m a -> m b) -> NameT m a -> NameT m b
mapNameT f m = NameT . StateT $ \s -> do
  (a, s) <- runStateT (unNameT m) s
  b <- f $ pure a
  pure (b, s)

-- MTL instances
instance (MonadIO m) => MonadIO (NameT m) where
  liftIO = lift . liftIO

instance (MonadError e m) => MonadError e (NameT m) where
  throwError = lift . throwError

  catchError m h = NameT $ catchError (unNameT m) (unNameT <$> h)

instance (MonadState s m) => MonadState s (NameT m) where
  get = lift get
  put = lift . put

instance (MonadName m) => MonadName (ExceptT e m) where
  freshVName = lift freshVName
  refreshVName = lift . refreshVName
  mkVName = lift . mkVName
  mkVDiscarded = lift mkVDiscarded

  mkMName = lift mkMName

instance (MonadName m) => MonadName (StateT e m) where
  freshVName = lift freshVName
  refreshVName = lift . refreshVName
  mkVName = lift . mkVName
  mkVDiscarded = lift mkVDiscarded

  mkMName = lift mkMName

instance (MonadName m) => MonadName (ReaderT e m) where
  freshVName = lift freshVName
  refreshVName = lift . refreshVName
  mkVName = lift . mkVName
  mkVDiscarded = lift mkVDiscarded

  mkMName = lift mkMName
