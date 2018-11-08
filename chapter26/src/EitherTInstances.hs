{-# LANGUAGE InstanceSigs #-}
module EitherTInstances where

import Control.Monad
import Control.Monad.Trans.Class

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

-- 1
instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT meea) = EitherT $ (fmap . fmap) f meea

-- 2
instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure

  (EitherT mef) <*> (EitherT mea) = EitherT $ (<*>) <$> mef <*> mea

-- 3
instance Monad m => Monad (EitherT e m) where
  return = pure

  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT mea) >>= f = EitherT $ do
    e <- mea
    case e of
      -- Remember, you can't use and return an as-pattern
      -- here because the compiler will interpret that
      -- as having the type Either e a, when we want
      -- Either e b. 
      (Left x) -> return $ Left x
      (Right x) -> runEitherT $ f x

-- 4
swapEither :: Either e a -> Either a e
swapEither (Right x) = Left x
swapEither (Left x) = Right x

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT me) = EitherT $ swapEither <$> me

-- 5
eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT aToC bToC (EitherT me) = me >>= either aToC bToC

-- Exercises: Lift More

-- 1
instance MonadTrans (EitherT e) where
  lift ma = EitherT $ liftM Right ma