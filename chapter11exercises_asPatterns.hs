import Data.Char (toUpper)
-- 1
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf sub@(x:xs) (y:ys)
  | x == y = True && isSubseqOf xs ys
  | otherwise = isSubseqOf sub ys

-- 2
capitalizeWords :: String -> [(String, String)]
capitalizeWords str = map capPair $ words str
  where
    capPair :: String -> (String, String)
    capPair word@(w:ord) = (word, toUpper w:ord)
