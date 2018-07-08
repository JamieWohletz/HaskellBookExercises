module VariationsOnEither where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data V e a =
    F e
  | S a
  deriving (Eq, Show)

instance Functor (V e) where
  fmap _ (F e) = F e
  fmap f (S a) = S $ f a

instance Monoid e => Applicative (V e) where
  pure = S
  (S f) <*> (S a) = S $ f a
  (F x) <*> (S _) = F x
  (S _) <*> (F x) = F x
  (F x) <*> (F y) = F $ mappend x y

instance (Arbitrary a, Arbitrary e) => Arbitrary (V e a) where
  arbitrary = oneof [pure S <*> arbitrary, pure F <*> arbitrary]

instance (Eq a, Eq e) => EqProp (V e a) where
  (=-=) = eq

verifyValidationApplicative :: IO ()
verifyValidationApplicative = quickBatch (applicative (S ("one", "two", "three") :: V String (String, String, String)))
