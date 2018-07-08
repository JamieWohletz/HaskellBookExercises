import Data.Monoid (First(..), Product(..), Sum(..))

-- 1

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

-- 2

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

-- 3

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = foldr (\a b -> b || a == x) False

-- 4

first :: Foldable t => t a -> Maybe a
first = getFirst . (foldMap (First . Just))

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum xs = do
  x <- first xs
  pure $ foldr min x xs

-- 5

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum xs = do
  x <- first xs
  pure $ foldr max x xs

-- 6

null :: Foldable t => t a -> Bool
null xs = case first xs of
  Nothing -> True
  _ -> False

-- 7

length :: Foldable t => t a -> Int
length = foldr (\a b -> b + 1) 0

-- 8

toList :: Foldable t => t a -> [a]
toList = foldMap (:[])

-- 9

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

-- 10

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\a b -> f a `mappend` b) mempty
