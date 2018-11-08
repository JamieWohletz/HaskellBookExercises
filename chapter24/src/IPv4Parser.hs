module IPv4Parser where

import Data.Word
import Text.Trifecta
import Data.Bits (shift)

type IPv4Bytes = (Integer, Integer, Integer, Integer)

data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord, Show)

parseDecimalByte :: Parser Integer
parseDecimalByte = do
  byte <- integer
  if byte < 256 && byte >= 0 then
    return byte
  else
    unexpected "number out of range"

parseIPv4 :: Parser IPAddress
parseIPv4 = do
  byte1 <- parseDecimalByte
  char '.'
  byte2 <- parseDecimalByte
  char '.'
  byte3 <- parseDecimalByte
  char '.'
  byte4 <- parseDecimalByte
  return $ mkIPAddress (byte1, byte2, byte3, byte4)

mkIPAddress :: IPv4Bytes -> IPAddress
mkIPAddress (b1, b2, b3, b4) = IPAddress $ fromInteger (n1 + n2 + n3 + n4)
  where
    n1 = shift b1 24
    n2 = shift b2 16
    n3 = shift b3 8
    n4 = b4
