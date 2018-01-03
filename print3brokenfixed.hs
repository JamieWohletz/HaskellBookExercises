module Print3BrokenFixed where

printSecond :: String -> IO ()
printSecond = putStrLn

main :: IO ()
main = do
  putStrLn greeting
  printSecond greeting
  where greeting = "Yarrrr"
