module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

digits :: Int -> [Int]
digits n = digits' n []
  where
    digits' :: Int -> [Int] -> [Int]
    digits' num xs
      | num < 10 = num:xs
      | otherwise =
          digits' (num `div` 10) ((num `mod` 10):xs)

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
