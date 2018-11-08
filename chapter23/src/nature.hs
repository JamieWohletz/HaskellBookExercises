{-# LANGUAGE InstanceSigs #-}
module Nature where

newtype Nature s a =
  Nature { runNature :: s -> (a, s) }

nature :: (s -> (a, s)) -> Nature s a
nature = Nature

instance Functor (Nature s) where
  fmap :: (a -> b) -> Nature s a -> Nature s b
  fmap f (Nature g) = Nature $ \s -> let (a, s2) = g s in (f a, s2)

instance Applicative (Nature s) where
  pure :: a -> Nature s a
  pure a = Nature $ \s -> (a, s)

  (<*>) :: Nature s (a -> b) -> Nature s a -> Nature s b
  (Nature f) <*> (Nature g) =
    Nature $ \s ->
      let
        (func, s1) = f s
        (a, s2) = g s1
      in
        (func a, s2)

instance Monad (Nature s) where
  return = pure

  (>>=) :: Nature s a
        -> (a -> Nature s b)
        -> Nature s b
  (Nature f) >>= g =
    Nature $ \s ->
      let
        (a, s1) = f s
        (b, s2) = runNature (g a) s1
      in
        (b, s2)
