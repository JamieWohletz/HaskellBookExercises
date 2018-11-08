module Ciphers where

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
caesar places = (map $ shift (+) places 'A' 'Z')

unCaesar' :: IO ()
unCaesar' = do
  putStrLn "Enter the string to decrypt."
  toDecrypt <- getLine
  putStrLn $ unCaesar 5 toDecrypt

unCaesar :: Int -> String -> String
unCaesar places = (map $ shift (-) places 'A' 'Z')

type CipherKey = String
type Message = String
type EncryptedMessage = String

replace :: Message -> CipherKey -> String
replace msg key = go msg key 0
  where
    go "" _ _ = ""
    go (ch:chs) key i = newChar:go chs key newIndex
      where
        charOk = ch `elem` ['A'..'Z']
        newChar = if charOk then key !! i else ch
        newIndex = if charOk then (mod (i + 1) (length key)) else i

getShiftPairs :: Message -> CipherKey -> [(Char, Int)]
getShiftPairs msg key = map (\(ch, r) -> (ch, ord r - ord 'A')) charPairs
  where
    charPairs = zipWith (,) msg rs
    rs = replace capMsg capKey
    capMsg = map toUpper msg
    capKey = map toUpper key

vigenere :: Message -> CipherKey -> EncryptedMessage
vigenere msg key = map (\(ch, places) -> shift (+) places 'A' 'Z' ch) toShift
  where
    toShift = getShiftPairs msg key

unVigenere :: EncryptedMessage -> CipherKey -> Message
unVigenere enc key = map (\(ch, places) -> shift (-) places 'A' 'Z' ch) toUnshift
  where
    toUnshift = getShiftPairs enc key