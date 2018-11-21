{-# LANGUAGE QuasiQuotes #-}

module AltParsing where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta

type NumberOrString =
  Either Integer String

a = "blah"
b = "123"
c = "123blah789"

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

parseNos :: Parser NumberOrString
parseNos = do
  skipMany (oneOf "\n")
  -- Note: <|> is OR for parsers
  v <-    (Left <$> integer)
      <|> (Right <$> some letter)
  skipMany (oneOf "\n")
  return v

main = do
  let p f i = parseString f mempty i
  print $ p parseNos eitherOr