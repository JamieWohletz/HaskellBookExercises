module Chapter9Exercises_StandardFunctions where

myOr :: [Bool] -> Bool
myOr [] = False
myOr (b:bs) = b || myOr bs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) =
  if f x
    then True
    else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem el (x:xs) =
  if el == x
    then True
    else myElem el xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' a = myAny (==a)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish $ map f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

chooseBy :: Ordering -> (a -> a -> Ordering) -> [a] -> a -> a
chooseBy _ _ [] z = z
chooseBy o h (y:ys) z =
  if o == h y z
    then chooseBy o h ys y
    else chooseBy o h ys z

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = chooseBy GT f xs x

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = chooseBy LT f xs x

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
