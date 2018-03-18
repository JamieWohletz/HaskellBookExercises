data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing
  | otherwise = Just $ naturalIntToNat x
  where
    naturalIntToNat :: Integer -> Nat
    naturalIntToNat 0 = Zero
    naturalIntToNat x = Succ $ naturalIntToNat (x - 1)
