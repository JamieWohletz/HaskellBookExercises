import Test.QuickCheck
import Data.Semigroup
import Data.List.NonEmpty

-- #1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- #2

newtype Identity a = Identity a deriving (Show, Eq)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity $ x <> y

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssoc = Identity [Int] -> Identity [Int] -> Identity [Int] -> Bool

-- #3

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc = Two [Int] [Int] -> Two [Int] [Int] -> Two [Int] [Int] -> Bool

-- #4

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a1 b1 c1) <> (Three a2 b2 c2) = Three (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeIntLists = Three [Int] [Int] [Int]

type ThreeAssoc = ThreeIntLists -> ThreeIntLists -> ThreeIntLists -> Bool

-- #5

-- sigh
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a,
          Semigroup b,
          Semigroup c,
          Semigroup d) => Semigroup (Four a b c d) where
  (Four a1 b1 c1 d1) <> (Four a2 b2 c2 d2) =
    Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

instance (Arbitrary a,
          Arbitrary b,
          Arbitrary c,
          Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

type FourIntLists = Four [Int] [Int] [Int] [Int]
type FourAssoc = FourIntLists -> FourIntLists -> FourIntLists -> Bool
         
-- #6

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj False) <> _ = BoolConj False
  _ <> (BoolConj False) = BoolConj False
  _ <> _ = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = do
    b <- arbitrary
    return $ BoolConj b

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- #7

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj True) <> _ = BoolDisj True
  _ <> (BoolDisj True) = BoolDisj True
  _ <> _ = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = do
    b <- arbitrary
    return $ BoolDisj b

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- #8

data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Snd a) <> _ = Snd a
  _ <> (Snd a) = Snd a
  x <> _ = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = frequency
    [(1, arbitrary >>= (\a -> return $ Fst a)), (1, arbitrary >>= (\b -> return $ Snd b))]

type OrAssoc = Or Int Int -> Or Int Int -> Or Int Int -> Bool

-- #9

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance Show (Combine a b) where
  show _ = "Combine { unCombine = <function> }"

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine $ \a -> f a <> g a

-- coarbitrary???
instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

type CSum = Combine Int (Sum Int)

combineAssoc :: CSum -> CSum -> CSum -> Int -> Bool
combineAssoc a b c x =
  -- my head hurts
  unCombine (a <> (b <> c)) x == unCombine ((a <> b) <> c) x

-- #10

newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Show (Comp a) where
  show _ = "Comp { unComp = <function> }"

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp $ f . g

instance (Arbitrary a, CoArbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- arbitrary
    return $ Comp f

type CompInt = Comp Int

compAssoc :: CompInt -> CompInt -> CompInt -> Int -> Bool
compAssoc a b c x =
  unComp (a <> (b <> c)) x == unComp ((a <> b) <> c) x
  
------

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck combineAssoc
  quickCheck compAssoc
