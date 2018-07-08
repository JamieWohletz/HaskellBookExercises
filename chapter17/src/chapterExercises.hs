import Control.Applicative (liftA3)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
 fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance Applicative Pair where
  pure a = Pair a a
  (Pair f1 f2) <*> (Pair a1 a2) = Pair (f1 a1) (f2 a2)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = arbitrary >>= (\a -> pure $ Pair a a)

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

verifyPairApplicative :: IO ()
verifyPairApplicative = quickBatch (applicative (Pair ("1", "2", "3") ("1", "2", "3")))

-- 2

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a $ f b

instance Monoid a => Applicative (Two a) where
  pure b = Two mempty b
  (Two a1 f) <*> (Two a2 b) = Two (mappend a1 a2) $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

verifyTwoApplicative :: IO ()
verifyTwoApplicative = quickBatch (applicative (Two ("1", "2", "3") ("1", "2", "3")))

-- 3

data Three a b c = Three a b c deriving (Eq, Show)

-- ffs
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three mempty mempty c
  (Three a1 b1 f) <*> (Three a2 b2 c) = Three a3 b3 $ f c
    where
      a3 = mappend a1 a2
      b3 = mappend b1 b2

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq


verifyThreeApplicative :: IO ()
verifyThreeApplicative = quickBatch (applicative (Three ("1", "2", "3") ("1", "2", "3") ("1", "2", "3")))

-- 4

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance (Monoid a) => Applicative (Three' a) where
  pure b = Three' mempty b b
  (Three' a1 f1 f2) <*> (Three' a2 b1 b2) = Three' (mappend a1 a2) (f1 b1) (f2 b2) 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Three' x y y

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq


verifyThreePrimeApplicative :: IO ()
verifyThreePrimeApplicative = quickBatch (applicative (Three' ("1", "2", "3") ("1", "2", "3") ("1", "2", "3")))

-- Skipping the last two because c'mon.

-- Combinations

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (\s1 v s2 -> (s1, v, s2))
