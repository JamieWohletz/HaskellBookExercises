module Chapter9Exercises_DataChar where

import Data.Char

-- Data.Char

-- #1
-- isUpper :: Char -> Bool
-- toUpper :: Char -> Char

-- #2
onlyUppers :: String -> String
onlyUppers = filter isUpper

-- #3
capFirst :: String -> String
capFirst "" = ""
capFirst (x:xs) = toUpper x : xs

-- #4
caps :: String -> String
caps "" = ""
caps (a:abc) = toUpper a : caps abc

-- #5
firstCapped :: String -> Maybe Char
firstCapped "" = Nothing
firstCapped str = Just $ toUpper $ head str

-- #6
-- Can't write pointfree pattern matches, so
-- this is now an unsafe function.
unsafeFirstCapped' :: String -> Char
unsafeFirstCapped' = toUpper . head
