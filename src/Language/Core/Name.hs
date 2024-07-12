{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Core.Name
  ( Name (..),
    MonadName(..),
    NameT,
    runNameT,
  )
where

import Control.Monad.Except (ExceptT, MonadError, catchError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT, evalStateT, get, modify)
import Control.Monad.Trans (MonadTrans, lift)
import Text.Printf (printf)

data Name
  = Name Int
  | AnnotatedName String Int
  | Discarded Int
  deriving (Eq, Ord)

instance Show Name where
  show (Name id) = printf "$%d" id
  show (AnnotatedName ann id) = printf "%s$%d" ann id
  show (Discarded id) = printf "discarded$%d" id

class (Monad m) => MonadName m where
  fresh :: m Name

  freshHint :: Name -> m Name

  mkName :: String -> m Name
  mkDiscarded :: m Name

newtype NameT m a = NameT {unNameT :: StateT Int m a}
  deriving (MonadTrans, Monad, Applicative, Functor)

freshId :: (Monad m) => NameT m Int
freshId = NameT $ do
  id <- get
  modify (+ 1)
  pure id

instance (Monad m) => MonadName (NameT m) where
  fresh = Name <$> freshId

  freshHint (Name _) = Name <$> freshId
  freshHint (AnnotatedName ann _) = AnnotatedName ann <$> freshId
  freshHint (Discarded _) = Discarded <$> freshId

  mkName ann = AnnotatedName ann <$> freshId
  mkDiscarded = Discarded <$> freshId

runNameT :: (Monad m) => NameT m a -> m a
runNameT = flip evalStateT 0 . unNameT

-- MTL instances
instance (MonadIO m) => MonadIO (NameT m) where
  liftIO = lift . liftIO

-- liftCatch :: Catch e m (a,s) -> Catch e (StateT s m) a
-- liftCatch catchE m h =
--     StateT $ \ s -> runStateT m s `catchE` \ e -> runStateT (h e) s

instance (MonadError e m) => MonadError e (NameT m) where
  throwError = lift . throwError

  catchError m h = NameT $ catchError (unNameT m) (unNameT <$> h)

instance (MonadName m) => MonadName (ExceptT e m) where
  fresh = lift fresh
  freshHint = lift . freshHint
  mkName = lift . mkName
  mkDiscarded = lift mkDiscarded

instance (MonadName m) => MonadName (ReaderT e m) where
  fresh = lift fresh
  freshHint = lift . freshHint
  mkName = lift . mkName
  mkDiscarded = lift mkDiscarded
