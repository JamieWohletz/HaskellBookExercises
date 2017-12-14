-- Let's write code
-- #1

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

tensDigit' = snd . dm10 . fst . dm10
  where dm10 = (`divMod` 10)

hunsD = snd . (`divMod` 10) . fst . (`divMod` 100)

-- #2

foldBool :: a -> a -> Bool -> a
foldBool a1 a2 b =
  case b of
    True -> a2
    False -> a1

foldBool' :: a -> a -> Bool -> a
foldBool' a1 a2 b
  | b == True = a2
  | b == False = a1

-- #3

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
