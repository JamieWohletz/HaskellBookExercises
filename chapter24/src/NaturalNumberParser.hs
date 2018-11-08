module NaturalNumberParser where

import Text.Trifecta
import Data.Char (digitToInt)

digitToInteger :: Char -> Integer
digitToInteger = toInteger . digitToInt

parseDigit :: Parser Char
parseDigit = oneOf ['0','1','2','3','4','5','6','7','8','9'] <?> "integer from 0-9"

base10Integer :: Parser Integer
base10Integer = parseIt <?> "base-10 integer"
  where
    parseIt :: Parser Integer
    parseIt = do
      digits <- some parseDigit
      return $ foldl (\b a -> (b * 10) + (digitToInteger a)) 0 digits
