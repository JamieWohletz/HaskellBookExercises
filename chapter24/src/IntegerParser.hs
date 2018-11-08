module IntegerParser where

import Text.Trifecta
import NaturalNumberParser

base10Integer' :: Parser Integer
base10Integer' = do
  sign <- option '+' (char '-')
  number <- base10Integer
  if sign == '-' then
    return (negate number)
  else
    return number