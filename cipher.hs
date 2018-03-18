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
        zeroedEnd = (ord lastCh + 1) - scalar

caesar' :: IO ()
caesar' = do
  putStrLn "Enter the string to encrypt."
  str <- getLine
  putStrLn $ caesar 5 str

caesar :: Int -> String -> String
caesar places = (map $ shift (+) places 'A' 'Z') . upper

unCaesar' :: IO ()
unCaesar' = do
  putStrLn "Enter the string to decrypt."
  toDecrypt <- getLine
  putStrLn $ unCaesar 5 toDecrypt

unCaesar :: Int -> String -> String
unCaesar places = (map $ shift (-) places 'A' 'Z') . upper
