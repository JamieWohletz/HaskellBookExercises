module PalindromeIO where

import Control.Monad (forever)
import System.Exit (exitSuccess)
import Data.Char (isLetter, toLower)

palindromic :: String -> Bool
palindromic s = let normalized = (map toLower . filter isLetter) s
                in normalized == reverse normalized

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case palindromic line1 of
    True -> putStrLn "It's a palindrome!"
    False -> putStrLn "Nope." *> exitSuccess
