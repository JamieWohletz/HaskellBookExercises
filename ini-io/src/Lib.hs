{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib (parseIniString, maybeSuccess) where

import Control.Applicative
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
-- parsers 0.12.3, trifecta 1.5.2
import Text.Trifecta

type Name = String
type Value = String
type Assignments = Map Name Value

newtype Header =
  Header String
  deriving (Eq, Ord, Show)

data Section =
  Section Header Assignments
  deriving (Eq, Show)

newtype Config =
  Config (Map Header Assignments)
  deriving (Eq, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p =
  char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader =
  parseBracketPair (Header <$> some letter)

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  _ <- char '='
  val <- some (noneOf "\n")
  -- The reason for skipEOL here
  -- is so that we can use parseAssignment
  -- to parse multiple assignment statements
  -- in a row, even if they're separated
  -- by EOLs
  skipEOL
  return (name, val)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipComments :: Parser ()
skipComments =
  skipMany (do _ <- char ';' <|> char '#'
               skipMany (noneOf "\n")
               skipEOL)

skipWhitespace :: Parser ()
skipWhitespace =
  skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $
    Section h (M.fromList assignments)

rollup :: Section
       -> Map Header Assignments
       -> Map Header Assignments
rollup (Section h a) m =
  M.insert h a m

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections =
        foldr rollup M.empty sections
  return (Config mapOfSections)

parseIniString :: String -> Result Config
parseIniString str = parseString parseIni mempty str

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing