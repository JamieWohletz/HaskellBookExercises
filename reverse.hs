module Reverse where

rvrs :: String -> String
rvrs s = awesome ++ is ++ curry 
  where 
    awesome = drop 9 s 
    is = take 4 $ drop 5 s
    curry = take 5 s

main :: IO ()
main = print $ rvrs "Curry is awesome"