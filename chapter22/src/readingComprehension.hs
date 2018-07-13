{-# LANGUAGE InstanceSigs #-}
-- 1

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f m1 m2 = f <$> m1 <*> m2

-- 2

newtype Reader r a =
  Reader { runReader :: r -> a }

asks :: (r -> a) -> Reader r a
asks ra = Reader ra

-- 3

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \r -> a

  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> rab r $ ra r
