module Reverse where

rvrs :: String -> String
rvrs str = thirdWord ++ " " ++ secondWord ++ " " ++ firstWord
  where
    firstWord = take 5 str
    secondWord = take 2 $ drop 6 str
    thirdWord = take 7 $ drop 9 str

main :: IO ()
main = print $ rvrs "Curry is awesome"
