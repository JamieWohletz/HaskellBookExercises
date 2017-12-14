eftBool :: Bool -> Bool -> [Bool]
eftBool True False = []
eftBool True True = [True]
eftBool False True = [False, True]
eftBool False False = [False]

fromTo :: (Ord a, Eq a, Enum a) => a -> a -> [a]
fromTo x y = fromTo' x y []
  where
    fromTo' x1 y1 xs
      | x1 > y1 = xs
      | x1 == y1 = xs ++ [x1]
      | otherwise = fromTo' (succ x1) y1 (xs ++ [x1])

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd o1 o2 = fromTo o1 o2

eftInt :: Int -> Int -> [Int]
eftInt x y = fromTo x y

eftChar :: Char -> Char -> [Char]
eftChar = fromTo
