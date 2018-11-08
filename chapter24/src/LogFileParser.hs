{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module LogFileParser where

import Text.Trifecta
import Control.Applicative
import qualified Data.Map.Strict as M
import Text.RawString.QQ
import qualified Test.QuickCheck as QC

import Data.Either (isRight)

stoi :: String -> Int
stoi = read

zeroLeftPad :: Int -> String -> String
zeroLeftPad desiredLength str = (take zeroCount (repeat '0')) ++ str
  where
    zeroCount = desiredLength - length str

data Time = Time { hours :: Int, minutes :: Int }

instance Show Time where
  show Time { hours, minutes } =
    let hourStr = zeroLeftPad 2 $ show hours
        minStr = zeroLeftPad 2 $ show minutes
    in hourStr ++ ":" ++ minStr

newtype Entry = Entry (Time, String)

instance Show Entry where
  show (Entry (t, s)) =
    show t ++ " " ++ s

data ISODate = ISODate {
                         year :: Int,
                         month :: Int,
                         day :: Int 
                       }
                       deriving Eq

instance Show ISODate where
  show ISODate { year, month, day } =
    show year ++ "-" ++ monthStr ++ "-" ++ dayStr
    where
      monthStr = zeroLeftPad 2 $ show month
      dayStr = zeroLeftPad 2 $ show day

showDateAsHeader :: ISODate -> String
showDateAsHeader date = "# " ++ show date

instance Ord ISODate where
  compare (ISODate { year = y1, month = m1, day = d1 })
          (ISODate { year = y2, month = m2, day = d2 })
          | y1 > y2 || y1 == y2 && m1 > m2 || y1 == y2 && m1 == m2 && d1 > d2 = GT
          | y1 == y2 && m1 == m2 && d1 == d2 = EQ
          | y1 < y2 || y1 == y2 && m1 < m2 || y1 == y2 && m1 == m2 && d1 < d2 = LT

newtype Log = Log (M.Map ISODate [Entry])

instance Show Log where
  show (Log m) = M.foldrWithKey f "" m
    where
      f :: ISODate -> [Entry] -> String -> String
      f date entries s =
        showDateAsHeader date
        ++ "\n"
        ++ (foldr (\entry str -> show entry ++ "\n" ++ str) "" entries)
        ++ "\n"
        ++ s

instance Semigroup Log where
  (Log m1) <> (Log m2) = Log $ m1 <> m2

instance Monoid Log where
  mempty = Log mempty

mkLog :: [(ISODate, [Entry])] -> Log
mkLog xs = Log $ M.fromList xs

verboseGuard :: (Monad m) => String -> Bool -> m ()
verboseGuard _ True = return ()
verboseGuard message False = fail message

skipEOL :: Parser ()
skipEOL = skipMany newline

ignoreComment :: Parser ()
ignoreComment = symbol "--" >> skipMany (noneOf "\n") >> skipEOL

skipComments :: Parser ()
skipComments = skipMany ignoreComment

skipWhitespace :: Parser ()
skipWhitespace =
  skipMany (char ' ' <|> char '\n')

skipIrrelevant :: Parser ()
skipIrrelevant = skipWhitespace >> skipComments >> skipWhitespace

parseDate :: Parser ISODate
parseDate = do
  year <- count 4 digit
  char '-'
  month <- count 2 digit
  char '-'
  day <- count 2 digit
  return $ ISODate (stoi year) (stoi month) (stoi day)

parseDateHeader :: Parser ISODate
parseDateHeader = do
  symbol "#"
  parseDate

parseTime :: Parser Time
parseTime = do
  hours <- count 2 digit
  char ':'
  minutes <- count 2 digit
  return $ Time (stoi hours) (stoi minutes)

parseEntry :: Parser Entry
parseEntry = do
  t <- parseTime
  char ' '
  msg <- many $ noneOf "\n-"
  return $ Entry (t, msg)

parseDay :: Parser Log
parseDay = do
  skipIrrelevant
  date <- parseDateHeader <* skipIrrelevant
  entries <- many $ (parseEntry <* skipIrrelevant)
  return $ mkLog [(date, entries)]

parseLog :: Parser Log
parseLog = do
  days <- many parseDay
  return $ foldr mappend mempty days

dateHeader = "# 2025-12-35\n"
entry = "08:00 Ate breakfast\n"
comment = "-- hey there, I'm a comment"
