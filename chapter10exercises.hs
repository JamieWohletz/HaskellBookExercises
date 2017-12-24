-- Warm-up and review
-- 1
-- a)
svs :: [Char] -> [Char] -> [(Char, Char, Char)]
svs stops vowels =
  [(stop1, vowel, stop2) | stop1 <- stops, vowel <- vowels, stop2 <- stops]

-- b)
svs' stops vowels =
  [(stop1, vowel, stop2) |
    stop1 <- stops,
    vowel <- vowels,
    stop2 <- stops,
    stop1 == 'p']

-- c)
nvn :: [String] -> [String] -> [(String, String, String)]
nvn nouns verbs = 
  [(noun1, verb, noun2) | noun1 <- nouns, verb <- verbs, noun2 <- nouns]

-- 2
-- seekritFunc returns the average word length for the words in a given sentence

-- 3
avgWordLength :: Fractional a => String -> a
avgWordLength str = lengthSum / wordCount
  where
    wordCount = fromIntegral $ length $ words str
    lengthSum = fromIntegral $ sum $ map length (words str)

-- Rewriting functions using folds
-- 1
myOr :: [Bool] -> Bool
myOr = foldr (||) False
-- 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> f a || b) False

-- 3
-- folding version
myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\a b -> a == x || b) False
-- any version
myElem' :: Eq a => a -> [a] -> Bool
myElem' a = any (==a)

-- 4
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 5
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a:b) []

-- 6
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\a b -> if p a then a : b else b) []

-- 7
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . myMap f

-- 9
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl (\b a -> if f a b == GT then a else b) x xs

-- 11
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl (\b a -> if f b a == LT then b else a) x xs
