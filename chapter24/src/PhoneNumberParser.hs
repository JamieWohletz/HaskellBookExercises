module PhoneNumberParser where

import Text.Trifecta
import Control.Monad
import Data.Maybe
import Control.Applicative

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea
              Exchange LineNumber
  deriving (Eq, Show)

zeroThruNine = ['0','1','2','3','4','5','6','7','8','9']

subList :: Int -> Int -> [a] -> [a]
subList lo hi = take (hi - lo) . drop lo

toInt :: String -> Int
toInt s = read s :: Int

mkPhoneNumber :: Int -> Int -> Int -> Maybe PhoneNumber
mkPhoneNumber n1 n2 n3 = do
  guard $ length (show n1) == 3
  guard $ length (show n2) == 3
  guard $ length (show n3) == 4
  return $ PhoneNumber n1 n2 n3

pn :: Int -> Int -> Int -> Parser PhoneNumber
pn n1 n2 n3 = do
  let number = mkPhoneNumber n1 n2 n3
  guard $ isJust number
  return $ fromJust number

parseDashSeparatedInts :: Parser [Int]
parseDashSeparatedInts = sepBy integer (char '-') >>= return . map fromInteger

parseDashSeparated :: Parser PhoneNumber
parseDashSeparated = do
  numbers <- parseDashSeparatedInts
  guard $ length numbers <= 4
  -- drop the trunk code if there is one
  let parts = dropWhile (<10) numbers
  guard $ length parts == 3
  let n1 = parts !! 0
  let n2 = parts !! 1
  let n3 = parts !! 2
  pn n1 n2 n3

parseNoSeparated :: Parser PhoneNumber
parseNoSeparated = do
  n <- integer <* eof
  let sn = show n
  guard $ length sn == 10
  let n1 = toInt $ subList 0 3 sn
  let n2 = toInt $ subList 3 6 sn
  let n3 = toInt $ subList 6 10 sn
  pn n1 n2 n3

parseParenedNPA :: Parser NumberingPlanArea
parseParenedNPA = do
  char '('
  npa <- integer
  guard $ length (show npa) == 3
  char ')'
  return (fromInteger npa)

-- I couldn't come up with a better name.
-- parses (XXX) XXX-XXXX
parseSpecial :: Parser PhoneNumber
parseSpecial = do
  npa <- parseParenedNPA
  space
  numbers <- parseDashSeparatedInts
  guard $ length numbers == 2
  pn npa (head numbers) (last numbers)

parsePhoneNumber :: Parser PhoneNumber
parsePhoneNumber =
  (try parseNoSeparated <|> try parseDashSeparated <|> parseSpecial)
  <?> "valid phone number"

ts1 = "303-529-1920"
ts2 = "1-303-529-1920"
ts3 = "(303) 529-1920"
ts4 = "3035291920"
ts5 = "asf23523"
ts6 = "930-33333-590"
ts7 = "(930)-333-5903"

test = do
  let t = print . parseString parsePhoneNumber mempty
  t ts1
  t ts2
  t ts3
  t ts4
  t ts5
  t ts6
  t ts7
