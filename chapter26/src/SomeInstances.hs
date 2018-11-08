{-# LANGUAGE InstanceSigs, GeneralizedNewtypeDeriving #-}

module SomeInstances where

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

-- 1
newtype MT m a =
  MT (MaybeT m a)
  -- Thank you, GeneralizedNewtypeDeriving!
  deriving (Functor, Applicative, Monad)

instance (MonadIO m) => MonadIO (MT m) where
  liftIO :: IO a -> MT m a
  liftIO ioa = MT (MaybeT $ pure <$> liftIO ioa)

-- 2
newtype RT r m a =
  RT (ReaderT r m a)
  deriving (Functor, Applicative, Monad)

instance (MonadIO m) => MonadIO (RT r m) where
  liftIO :: IO a -> RT r m a
  liftIO ioa = RT $ ReaderT $ \r -> liftIO ioa

-- 3
newtype ST s m a =
  ST (StateT s m a)
  deriving (Functor, Applicative, Monad)

instance (MonadIO m) => MonadIO (ST s m) where
  liftIO :: IO a -> ST s m a
  liftIO ioa = ST $ StateT $ \s -> (,) <$> liftIO ioa <*> pure s