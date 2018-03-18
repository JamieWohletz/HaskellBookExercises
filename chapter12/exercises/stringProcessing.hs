import Data.Char (toLower)
import Data.List (intercalate)
-- 1
lower :: String -> String
lower = map toLower

replaceInsensitive :: String -> String -> String -> String
replaceInsensitive pattern replacement str
  | lower str == lower pattern = replacement
  | otherwise = str

replaceThe :: String -> String
replaceThe = (intercalate " ") . map (replaceInsensitive "the" "a") . words

-- oops, supposed to write it recursively
replaceThe' :: String -> String
replaceThe' str = r str "" ""
  where
    r :: String -> String -> String -> String
    r "" lastWord newStr = newStr ++ repl lastWord
    r (' ':xs) lastWord newStr = r xs "" (newStr ++ repl lastWord ++ " ")
    r (x:xs) lastWord newStr = r xs (lastWord ++ [x]) newStr

    repl word = replaceInsensitive "the" "a" word

-- 2
isThe :: String -> Bool
isThe s = lower s == "the"

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = go str "" 0
  where
    go :: String -> String -> Integer -> Integer
    go "" _ c = c
    go (' ':ch:chs) lastWord c =
      let
        newC = if isThe lastWord && isVowel ch then c + 1 else c
      in
        go (ch:chs) "" newC
    go (x:xs) lastWord c = go xs (lastWord ++ [x]) c

-- 3
countBy :: (a -> Bool) -> [a] -> Integer
countBy _ [] = 0
countBy p (x:xs) = adder + countBy p xs
  where
    adder = if p x then 1 else 0

countVowels :: String -> Integer
countVowels = countBy isVowel
