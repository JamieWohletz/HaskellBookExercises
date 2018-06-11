module ListApplicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a xs) = Cons (f a) $ fmap f xs

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  fs <*> as = flatMap (\f -> fmap f as) fs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = arbitrary >>= (pure . pure)

instance Eq a => EqProp (List a) where
  (=-=) = eq

verifyListApplicative :: IO ()
verifyListApplicative = quickBatch (applicative (Cons ("one", "two", "three") Nil))

fromList :: [a] -> List a
fromList = foldr (\x xs -> Cons x xs) Nil

zippy :: List a -> List b -> List (a, b)
zippy Nil _ = Nil
zippy _ Nil = Nil
zippy (Cons a as) (Cons b bs) = Cons (a, b) $ zippy as bs

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _ = Nil
take' x (Cons a as) = Cons a $ take' (x - 1) as

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
  Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f xs = concat' $ fmap f xs

repeat' :: a -> List a
repeat' a = Cons a $ repeat' a
