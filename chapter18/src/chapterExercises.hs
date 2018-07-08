import Control.Monad (join)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Monad instances

-- 1
data Nope a =
  NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = pure NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

validateNope = do
  let naw :: Nope (Int, Int, Int)
      naw = undefined
  quickBatch $ functor naw
  quickBatch $ applicative naw
  quickBatch $ monad naw

-- 2
-- Not using "PhhhbbtttEither" >,..,<
data YeahNaw b a =
    Yeah a
  | Naw b
  deriving (Eq, Show)

instance Functor (YeahNaw b) where
  fmap _ (Naw b) = Naw b
  fmap f (Yeah a) = Yeah $ f a

instance Applicative (YeahNaw b) where
  pure = Yeah
  (Naw b) <*> _ = Naw b
  _ <*> (Naw b) = Naw b
  (Yeah f) <*> (Yeah a) = Yeah $ f a

instance Monad (YeahNaw b) where
  return = pure
  Naw x >>= _ = Naw x
  (Yeah a) >>= f = f a

instance (Arbitrary b, Arbitrary a) => Arbitrary (YeahNaw b a) where
  arbitrary = oneof
    [Yeah <$> arbitrary, Naw <$> arbitrary]

instance (Eq b, Eq a) => EqProp (YeahNaw b a) where
  (=-=) = eq

validateYeahNaw = do
  let yeahnaw :: YeahNaw (Int, Int, Int) (Int, Int, Int)
      yeahnaw = undefined
  quickBatch $ functor yeahnaw
  quickBatch $ applicative yeahnaw
  quickBatch $ monad yeahnaw

-- good gawd, the instances for dayyyyyyyyyyz
-- 3

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity $ f a

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

validateIdentity = do
  let iden :: Identity (Int, Int, Int)
      iden = undefined
  quickBatch $ functor iden
  quickBatch $ applicative iden
  quickBatch $ monad iden

-- 4

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

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

-- Yanked these from the chapter17 list applicative exercise
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a xs) = Cons (f a) $ fmap f xs

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  fs <*> as = flatMap (\f -> fmap f as) fs

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  xs >>= f = concat' $ fmap f xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = arbitrary >>= (pure . pure)

instance Eq a => EqProp (List a) where
  (=-=) = eq

validateList = do
  let list :: List (Int, Int, Int)
      list = undefined
  quickBatch $ functor list
  quickBatch $ applicative list
  quickBatch $ monad list

-- Write the following functions...

-- 1

j :: Monad m => m (m a) -> m a
j m = m >>= id

-- 2

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m = f <$> m

-- 3

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f m1 m2 = f <$> m1 <*> m2

-- 4

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

-- 5

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
-- look at dat applicative style doe
meh (x:xs) f = (++) <$> (pure <$> f x) <*> meh xs f

-- 6

flipType :: (Monad m) => [m a] -> m [a]
flipType ms = meh ms id
