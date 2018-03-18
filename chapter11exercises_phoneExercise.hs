module PhoneExercise where

import Data.Char (toUpper, isLetter)
import Data.List (maximumBy)


-- 1
data DigitOrSymbol = Digit Int | Sym Char deriving Show
data DaPhone = DaPhone [(DigitOrSymbol, [Char])] deriving Show

phone :: DaPhone
phone = DaPhone
  [ (Digit 1, ""), (Digit 2, "ABC"), (Digit 3, "DEF"),
    (Digit 4, "GHI"), (Digit 5, "JKL"), (Digit 6, "MNO"),
    (Digit 7, "PQRS"), (Digit 8, "TUV"), (Digit 9, "WXYZ"),
    (Sym '*', ""), (Digit 0, " "), (Sym '#', ".,")]

-- 2
type Presses = Int

index :: Eq a => a -> [a] -> Int
index a xs  = go a xs 0
  where
    go _ [] _ = -1
    go a (x:xs) i = if a == x then i else go a xs (i + 1)

reverseTaps :: DaPhone -> Char -> [(DigitOrSymbol, Presses)]
reverseTaps (DaPhone keys) ch
  | isUppercaseLetter = [(Sym '*', 1), keyPress]
  | otherwise = [keyPress]
  where
    upper :: Char
    upper = toUpper ch

    isUppercaseLetter :: Bool
    isUppercaseLetter = isLetter ch && upper == ch

    matchesChars :: (DigitOrSymbol, [Char]) -> Bool
    matchesChars (_, chs) = upper `elem` chs

    (key, chars) = head $ filter matchesChars keys
    keyPress = (key, index upper chars + 1)

cellPhonesDead :: DaPhone -> String -> [(DigitOrSymbol, Presses)]
cellPhonesDead ph = concatMap (reverseTaps ph)

-- 3
fingerTaps :: [(DigitOrSymbol, Presses)] -> Presses
fingerTaps = foldr (\(_, n) sum -> n + sum) 0

-- 4
type Conversation = [String]
type CountPair a = (a, Int)

convo :: Conversation
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol",
   "Lol ya", "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Just making sure rofl ur turn"]

compareCountPairs :: Ord a => CountPair a -> CountPair a -> Ordering
compareCountPairs (_, x) (_, y) = compare x y

countUniques :: Eq a => [a] -> [CountPair a]
countUniques xs = go xs xs []
  where
    go _ [] _ = []
    go all (x:xs) counted
      | x `elem` counted = go all xs counted
      | otherwise = (x, count x all):go all xs (x:counted)

count :: Eq a => a -> [a] -> Int
count x xs = length $ filter (==x) xs


mostPopularLetter :: String -> Maybe Char
mostPopularLetter "" = Nothing
mostPopularLetter str =
  Just $ fst $ maximumBy compareCountPairs $ countUniques str

pressesForChar :: Char -> Presses
pressesForChar = fingerTaps . reverseTaps phone

popularityStats :: Conversation -> [Maybe (Char, Presses)]
popularityStats = map analyze
  where
    analyze str =
      let mostPop = mostPopularLetter str in
      case mostPop of
        Nothing -> Nothing
        (Just c) -> Just (c, pressesForChar c)
      
-- 5
coolestLtr :: Conversation -> Maybe Char
coolestLtr [] = Nothing
coolestLtr strs =
  Just $ fst $ maximumBy compareCountPairs $ concatMap countUniques strs

-- 6
coolestWord :: [String] -> Maybe String
coolestWord [] = Nothing
coolestWord strs =
  Just $ fst $ maximumBy compareCountPairs $ countUniques ws
  where
    ws = concatMap words strs
