module Print3Broken where

-- making greeting a top-level declaration to fix this module
greeting :: String
greeting = "Yarrrr"

printSecond :: IO ()
printSecond = do
  putStrLn greeting

main :: IO ()
main = do
  putStrLn greeting
  printSecond
