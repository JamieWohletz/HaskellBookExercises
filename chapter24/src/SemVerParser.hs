module SemVerParser where

import Text.Trifecta
import Control.Applicative

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving Show

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving Show

teststr1 = "1.35.9"
teststr2 = "93.2.89-alpha.3.25"
teststr3 = "2.3.5+horololo"
teststr4 = "5.3.12+hy.jk.xq"
teststr5 = "5.3.12-beta.9.1+hy.jk.xq"
teststr6 = "should fail because this isn't a semver"

parseNOS :: Parser NumberOrString
parseNOS = (NOSI <$> decimal) <|> (NOSS <$> (some letter))

parseRelease :: Parser Release
parseRelease = do
  char '-'
  sepBy parseNOS (char '.')

parseMetadata :: Parser Metadata
parseMetadata = do
  char '+'
  sepBy parseNOS (char '.')

parseOptionals :: Parser (Release, Metadata)
parseOptionals = do
  release <- option [] parseRelease
  metadata <- option [] parseMetadata
  return (release, metadata)

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- decimal
  char '.'
  minor <- decimal
  char '.'
  patch <- decimal
  (release, metadata) <- parseOptionals
  return $ SemVer major minor patch release metadata

test = do
  let runTest = parseString parseSemVer mempty
  print $ runTest teststr1
  print $ runTest teststr2
  print $ runTest teststr3
  print $ runTest teststr4
  print $ runTest teststr5
  print $ runTest teststr6
