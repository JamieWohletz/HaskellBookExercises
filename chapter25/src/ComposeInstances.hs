{-# LANGUAGE InstanceSigs #-}
module ComposeInstances where

import Twinplicative

instance (Foldable f, Foldable g) =>
         Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap aToM (Compose fga) =
    foldMap (\g -> foldMap aToM g) fga

instance (Traversable f, Traversable g) =>
         Traversable (Compose f g) where
  traverse :: Applicative h => (a -> h b) -> Compose f g a -> h (Compose f g b)
  traverse f (Compose fga) =
    Compose <$> traverse (\g -> traverse f g) fga

-- traverse f g == h (g b)
-- traverse (traverse f) fga == h (f g b)