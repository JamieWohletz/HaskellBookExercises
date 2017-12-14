-- Fixing dividedBy

-- original function
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

-- From the exercise, it looks like they only want the result of
-- the division now, and don't care about the remainder.
-- Here we go...
data Quotient a = Quotient a | DividedByZero deriving Show

dividedBy' :: Integer -> Integer -> Quotient Integer
dividedBy' numr denom = go posNumr posDenom 0
  where
    positiveResult = (numr < 0 && denom < 0) || (numr > 0 && denom > 0)
    posNumr = abs numr
    posDenom = abs denom
    go n d count
      | d == 0 = DividedByZero
      | n < d = Quotient $ if positiveResult then count else (-count)
      | otherwise = go (n - d) d (count + 1)

-- McCarthy 91 function

mc91 :: (Integral a) => a -> a
mc91 x
  | x > 100 = x - 10
  | otherwise = mc91 . mc91 $ x + 11
