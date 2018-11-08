module IPv6Parser where

import Text.Trifecta
import Data.Bits (shift)
import Data.Word
import Numeric (readHex)
import Data.List (any, elem)
import LogFileParser (zeroLeftPad)
import Data.Maybe (fromJust)
import Control.Applicative

type IPv6Hextets = (Integer, Integer, Integer, Integer, Integer, Integer, Integer)

data IPAddress6 =
  IPAddress6 Word64 Word64
  deriving (Eq, Ord)

data Characters = Colon | Hex | Any

data IPv6 =
  IPv6 { pieces :: [String], hasExpander :: Bool }
  deriving Show

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:xs) = Just x

ipv6 = IPv6 [] False

separator :: String -> Bool
separator s = s == ":" || s == "::"

-- I GIVE UP. I'm just not smart enough to parse a goddamn IPv6 address. Game over.
append :: String -> IPv6 -> Maybe IPv6
append ":" (IPv6 ps hasExpander) =
  case safeLast ps of
    Nothing -> Just $ IPv6 (ps ++ [":"]) False
    Just a -> case separator a of
                     True -> if hasExpander
                            then Nothing
                            else Just $ IPv6 (init ps ++ ["::"]) True
                     _  -> Just $ IPv6 (ps ++ [":"]) hasExpander
append x (IPv6 ps hasExpander) = Just $ IPv6 (ps ++ [x]) hasExpander

instance Show IPAddress6 where
 show (IPAddress6 w1 w2) = show $ (shift (toInteger w1) 64) + (toInteger w2)

-- basic representation:
-- x:x:x:x:x:x:x:x
-- each 'x' is one to four hexadecimal digits
-- (16 bits for each group)

hexToDecimal :: String -> Integer
hexToDecimal = fst . head . readHex

toIPv6 :: [String] -> IPAddress6
toIPv6 xs = IPAddress6 (toWord64 mostSigFour) (toWord64 leastSigFour)
  where
    mostSigFour = take 4 hexes
    leastSigFour = drop 4 hexes
    hexes = map (fst . head. readHex) xs
    toWord64 :: [Integer] -> Word64
    toWord64 (x1:x2:x3:x4:xs) = fromInteger $ x4 + (shift x3 16) + (shift x2 32) + (shift x1 48)

-- parseIPv6 :: Parser IPAddress6
-- parseIPv6 = do
--   hextets <- parseHextets
--   return $ toIPv6 hextets

parseHex :: Parser String
parseHex = many (oneOf $ ['a'..'f'] ++ ['0'..'9'] ++ ['A'..'F'])

-- maybe i have to use recursion here...
parseHextets :: Parser IPv6
parseHextets = go ipv6
  where
    go :: IPv6 -> Parser IPv6
    go ip =
      do
        next <- ((:[]) <$> char ':') <|> parseHex
        if next == "" then
          return ip
        else
          case append next ip of
            Nothing -> fail "Invalid IPv6 address"
            Just nextIp -> go nextIp

expand :: Int -> [String] -> [String]
expand _ [] = []
expand missing ("":xs)
  | missing == 0 = expand 0 xs
  | otherwise = map (const "0000") [1..missing `div` 4] ++ expand 0 xs
expand missing (x:xs) = x:expand missing xs