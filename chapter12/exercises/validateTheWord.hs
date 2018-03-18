newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

-- taken from stringProcessing.hs
isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

countBy :: (a -> Bool) -> [a] -> Integer
countBy _ [] = 0
countBy p (x:xs) = adder + countBy p xs
  where
    adder = if p x then 1 else 0
--

countVowelsAndConsonants :: String -> (Integer, Integer)
countVowelsAndConsonants str =
  (countBy isVowel str, countBy (not . isVowel) str)

mkWord :: String -> Maybe Word'
mkWord str =
  let (vs, cs) = countVowelsAndConsonants str in
  if vs > cs then Nothing else Just (Word' str)
