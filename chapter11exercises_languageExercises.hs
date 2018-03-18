import Data.Char (toUpper, isAlphaNum)

-- 1
capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (w:ord) = toUpper w:ord

-- 2
capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph para = (capitalizeWord s1) ++ (s2') ++ (capitalizeParagraph s2'')
  where
    (s2', s2'') = break (\ch -> ch /= '.' && ch /= ' ') s2
    (s1, s2) = break (=='.') para


