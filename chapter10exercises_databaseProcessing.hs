module DatabaseProcessing where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
             (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
             (fromGregorian 1921 5 1)
             (secondsToDiffTime 34123))
  ]

minDate :: UTCTime
minDate = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)

-- #1
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate items = map (\(DbDate x) -> x) $ filter isDate items
  where
    isDate :: DatabaseItem -> Bool
    isDate (DbDate _) = True
    isDate _ = False

-- #2
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber items = map (\(DbNumber x) -> x) $ filter isNumber items
  where
    isNumber :: DatabaseItem -> Bool
    isNumber (DbNumber _) = True
    isNumber _ = False

-- #3
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent items =
  foldr
    (\a b -> if a > b then a else b)
    minDate
    dates
  where
    dates = filterDbDate items

-- #4
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- #5
avgDb :: [DatabaseItem] -> Double
avgDb items = (fromIntegral $ sum nums) / (fromIntegral $ length nums)
  where
    nums = filterDbNumber items
