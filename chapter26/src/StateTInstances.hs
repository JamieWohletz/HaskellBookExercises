{-# LANGUAGE InstanceSigs #-}
module StateTInstances where

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

-- 1
instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT smas) = StateT $ (fmap . fmap) (\(a, s) -> (f a, s)) smas

-- 2
instance (Monad m) => Applicative (StateT r m) where
  pure a = StateT $ \s -> pure (a, s)
  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT smab) <*> (StateT sma) =
    StateT $ \s -> do
      (f, s1) <- smab s
      (a, s2) <- sma s1
      return (f a, s2)

-- 3
instance (Monad m) => Monad (StateT s m) where
  return = pure

  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT sma) >>= asmb = StateT $ \s -> do
    (a, s1) <- sma s
    runStateT (asmb a) s1