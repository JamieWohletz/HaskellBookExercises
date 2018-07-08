{-# LANGUAGE FlexibleContexts #-}

module SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a = S (n a) a deriving (Eq, Show)

instance ( Functor n
         , Arbitrary (n a)
         , Arbitrary a )
        => Arbitrary (S n a) where
  arbitrary =
    S <$> arbitrary <*> arbitrary

-- The EqProp instance the book gives you is wrong!
instance (Eq (n a), Eq a) => EqProp (S n a) where
  (=-=) = eq

instance Functor n => Functor (S n) where
  fmap f (S g a) = S (f <$> g) $ f a

instance Foldable n => Foldable (S n) where
  foldMap f (S g a) = foldMap f g `mappend` f a

instance Traversable n => Traversable (S n) where
  traverse f (S g a) = S <$> traverse f g <*> f a

main = do
  let trigger :: S [] (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)
