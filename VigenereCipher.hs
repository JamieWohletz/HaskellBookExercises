import Data.Char
-- taken from cipher.hs
shift :: (Int -> Int -> Int) -> Int -> Char -> Char -> Char -> Char
shift shifter places firstCh lastCh ch
  | not $ ch `elem` [firstCh..lastCh] = ch
  | otherwise =
      chr $ scalar + mod (shifter zeroedCh places) zeroedEnd
      where
        scalar = ord firstCh
        zeroedCh = ord ch - scalar
        zeroedEnd = (ord lastCh + 1) - scalar

type CipherKey = String
type Message = String
type EncryptedMessage = String

replace :: Message -> CipherKey -> String
replace msg key = go msg key 0
  where
    go "" _ _ = ""
    go (ch:chs) key i = newChar:go chs key newIndex
      where
        charOk = ch `elem` ['A'..'Z']
        newChar = if charOk then key !! i else ch
        newIndex = if charOk then (mod (i + 1) (length key)) else i

shiftPairs :: Message -> CipherKey -> [(Char, Int)]
shiftPairs msg key = map (\(ch, r) -> (ch, ord r - ord 'A')) charPairs
  where
    charPairs = zipWith (,) msg rs
    rs = replace capMsg capKey
    capMsg = map toUpper msg
    capKey = map toUpper key

vigenere :: Message -> CipherKey -> EncryptedMessage
vigenere msg key = map (\(ch, places) -> shift (+) places 'A' 'Z' ch) toShift
  where
    toShift = shiftPairs msg key
