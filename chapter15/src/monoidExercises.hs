import Test.QuickCheck
import Data.Semigroup
import Data.List.NonEmpty

-- #1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- #2

newtype Identity a =
  Identity a deriving (Show, Eq)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity $ x <> y

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

-- #4
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj False) <> _ = BoolConj False
  _ <> (BoolConj False) = BoolConj False
  _ <> _ = BoolConj True

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = do
    b <- arbitrary
    return $ BoolConj b

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- #5

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj True) <> _ = BoolDisj True
  _ <> (BoolDisj True) = BoolDisj True
  _ <> _ = BoolDisj False

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance Arbitrary BoolDisj where
  arbitrary = do
    b <- arbitrary
    return $ BoolDisj b

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- #6

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance Show (Combine a b) where
  show _ = "Combine { unCombine = <function> }"

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine $ \a -> f a <> g a

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
  mempty = Combine $ \_ -> mempty
  mappend = (<>)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

type CSum = Combine Int (Sum Int)

combineAssoc :: CSum -> CSum -> CSum -> Int -> Bool
combineAssoc a b c x =
  unCombine (a <> (b <> c)) x == unCombine ((a <> b) <> c) x

combineLeftIdentity :: Combine Int String -> Int -> Bool
combineLeftIdentity c x = unCombine (mempty `mappend` c) x == unCombine c x

combineRightIdentity :: Combine Int String -> Int -> Bool
combineRightIdentity c x = unCombine (c `mappend` mempty) x == unCombine c x

-- #7

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

-- #8

newtype Mem s a =
  Mem {
    runMem :: s -> (a, s)
  }

instance Show (Mem s a) where
  show _ = "Mem s a"

instance Semigroup a => Semigroup (Mem s a) where
  (<>) (Mem f) (Mem g) =
    Mem $ \s ->
      let (a1, s1) = f s
          (a2, s2) = g s1
      in
        (a1 <> a2, s2)

instance (Semigroup a, Monoid a) => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend = (<>)

instance (CoArbitrary a, Arbitrary a, Arbitrary b) => Arbitrary (Mem a b) where
  arbitrary = do
    f <- arbitrary
    return $ Mem f

type MemStrs = Mem String String

memAssoc :: MemStrs -> MemStrs -> MemStrs -> String -> Bool
memAssoc a b c x =
  runMem (a <> (b <> c)) x == runMem ((a <> b) <> c) x

memLeftIdentity :: MemStrs -> String -> Bool
memLeftIdentity c x = runMem (mempty `mappend` c) x == runMem c x

memRightIdentity :: MemStrs -> String -> Bool
memRightIdentity c x = runMem (c `mappend` mempty) x == runMem c x

------

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty `mappend` a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a `mappend` mempty) == a

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

main :: IO ()
main = do
  putStrLn "Trivial"
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  putStrLn "Identity a"
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  putStrLn "BoolConj"
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  putStrLn "BoolDisj"
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  putStrLn "Combine a b"
  quickCheck combineAssoc
  quickCheck combineLeftIdentity
  quickCheck combineRightIdentity
  putStrLn "Mem s a"
  quickCheck memAssoc
  quickCheck memLeftIdentity
  quickCheck memRightIdentity
