{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)

-- 1
instance TooMany (Int, String) where
  tooMany (i, _) = tooMany i

-- 2
instance TooMany (Int, Int) where
  tooMany (i1, i2) = tooMany $ i1 + i2

-- 3
instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n1, n2) = tooMany $ n1 + n2
