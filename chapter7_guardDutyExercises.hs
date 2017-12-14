avgGrade :: (Ord a, Fractional a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y < 0.59 = 'F'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  where y = x / 100
