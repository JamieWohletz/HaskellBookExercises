import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose' :: (Eq (f c), Functor f) =>
                   f a
                -> Fun a b
                -> Fun b c
                -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int

-- #1
newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

-- #2

data Pair a = Pair a a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    return (Pair x x)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

-- #3
data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Two x y)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

-- #4
data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three x y z)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

-- #5
data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Three' x y y)

instance Functor (Three' a) where
  fmap f (Three' x y1 y2) = Three' x (f y1) (f y2)

-- #6

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Arbitrary a,
          Arbitrary b,
          Arbitrary c,
          Arbitrary d) =>
          Arbitrary (Four a b c d) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    q <- arbitrary
    return (Four x y z q)

instance Functor (Four a b c) where
  fmap f (Four x y z q) = Four x y z $ f q

-- #7

data Four' a b = Four' a a a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Four' x x x y)

instance Functor (Four' a) where
  fmap f (Four' x1 x2 x3 y) = Four' x1 x2 x3 $ f y

-- #8

-- No, because the Trivial type constructor is a type _constant_,
-- and Functor requires a higher-kinded type.

runTests :: IO ()
runTests = do
  putStrLn "identity law for Identity Int"
  quickCheck (functorIdentity :: Identity Int -> Bool)
  putStrLn "composition law for Identity Int"
  quickCheck (functorCompose' :: Identity Int -> IntToInt -> IntToInt -> Bool)
  putStrLn "identity law for Pair Int"
  quickCheck (functorIdentity :: Pair Int -> Bool)
  putStrLn "composition law for Pair Int"
  quickCheck (functorCompose' :: Pair Int -> IntToInt -> IntToInt -> Bool)
  putStrLn "identity law for Two Int Int"
  quickCheck (functorIdentity :: Two Int Int -> Bool)
  putStrLn "composition law for Two Int Int"
  quickCheck (functorCompose' :: Two Int Int -> IntToInt -> IntToInt -> Bool)
  putStrLn "identity law for Three Int Int Int"
  quickCheck (functorIdentity :: Three Int Int Int -> Bool)
  putStrLn "composition law for Three Int Int Int"
  quickCheck (functorCompose' :: Three Int Int Int -> IntToInt -> IntToInt -> Bool)
  putStrLn "identity law for Three' Int Int"
  quickCheck (functorIdentity :: Three' Int Int -> Bool)
  putStrLn "composition law for Three' Int Int"
  quickCheck (functorCompose' :: Three' Int Int -> IntToInt -> IntToInt -> Bool)
  putStrLn "identity law for Four Int Int Int Int"
  quickCheck (functorIdentity :: Four Int Int Int Int -> Bool)
  putStrLn "composition law for Four Int Int Int Int"
  quickCheck (functorCompose' :: Four Int Int Int Int -> IntToInt -> IntToInt -> Bool)
  putStrLn "identity law for Four' Int Int"
  quickCheck (functorIdentity :: Four' Int Int -> Bool)
  putStrLn "composition law for Four' Int Int"
  quickCheck (functorCompose' :: Four' Int Int -> IntToInt -> IntToInt -> Bool)
