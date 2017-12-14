module Cipher where

import Data.Char

lower :: String -> String
lower = map toLower

upper :: String -> String
upper = map toUpper

shift :: (Int -> Int -> Int) -> Int -> Char -> Char -> Char -> Char
shift shifter places firstCh lastCh ch
  | not $ ch `elem` [firstCh..lastCh] = ch
  | otherwise =
      chr $ scalar + mod (shifter zeroedCh places) zeroedEnd
      where
        scalar = ord firstCh
        zeroedCh = ord ch - scalar
        zeroedEnd = ord lastCh - scalar

caesar :: Int -> String -> String
caesar places = (map $ shift (+) places 'A' 'Z') . upper

unCaesar :: Int -> String -> String
unCaesar places = (map $ shift (-) places 'A' 'Z') . upper
