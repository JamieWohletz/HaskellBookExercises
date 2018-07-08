import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--Identity

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  sequenceA (Identity m) = fmap Identity m

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

verifyIdentity = do
  let trigger :: Identity (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)

-- Constant

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  sequenceA (Constant a) = pure $ Constant a

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = fmap Constant arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

verifyConstant = do
  let trigger :: Constant String (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)

-- Maybe

data Optional a =
    Nada
  | Yep a
  deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  sequenceA Nada = pure Nada
  sequenceA (Yep m) = fmap Yep m

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, pure Nada), (1, Yep <$> arbitrary)]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

verifyOptional = do
  let trigger :: Optional (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)

-- List

-- ok, it's time to start grabbing instances from previous chapter exericses
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
  Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f xs = concat' $ fmap f xs

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a xs) = Cons (f a) $ fmap f xs

instance Foldable List where
  foldr _ b Nil = b
  foldr f b (Cons a xs) = f a (foldr f b xs)

instance Traversable List where
  sequenceA Nil = pure Nil
  sequenceA (Cons m ms) = Cons <$> m <*> sequenceA ms

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, pure Nil), (1, (`Cons` Nil) <$> arbitrary)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

verifyList = do
  let trigger :: List (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)

-- Three

-- herrrre we go.

data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

-- Thank you, Chapter 20
instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  sequenceA (Three a b m) = Three a b <$> m

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

verifyThree = do
  let trigger :: Three Int Int (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)

-- Pair

-- SO MANY INSTANCES

data Pair a b =
  Pair a b
  deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a $ f b

instance Foldable (Pair a) where
  foldMap f (Pair a b) = f b

instance Traversable (Pair a) where
  sequenceA (Pair a m) = Pair a <$> m

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

verifyPair = do
  let trigger :: Pair Int (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)

-- Big

data Big a b =
  Big a b b
  deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a b1 b2) = Big a (f b1) (f b2)

instance Foldable (Big a) where
  foldMap f (Big a b1 b2) = f b1 `mappend` f b2

instance Traversable (Big a) where
  sequenceA (Big a m1 m2) = Big a <$> m1 <*> m2

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

verifyBig = do
  let trigger :: Big Int (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)

data Bigger a b =
  Bigger a b b b
  deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b1 b2 b3) = Bigger a (f b1) (f b2) (f b3)

instance Foldable (Bigger a) where
  foldMap f (Bigger a m1 m2 m3) = f m1 `mappend` f m2 `mappend` f m3

instance Traversable (Bigger a) where
  sequenceA (Bigger a m1 m2 m3) = Bigger a <$> m1 <*> m2 <*> m3

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
