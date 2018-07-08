import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid ((<>))

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node t1 a t2) = Node (fmap f t1) (f a) (fmap f t2)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node t1 a t2) = (foldMap f t1) <> f a <> (foldMap f t2) 

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node t1 a t2) = Node <$> traverse f t1 <*> f a <*> traverse f t2

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = oneof
                [
                  pure Empty,
                  Leaf <$> arbitrary,
                  Node <$> arbitrary <*> arbitrary <*> arbitrary
                ]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

verify = do
  let trigger :: Tree (String, String, String)
      trigger = undefined
  quickBatch (traversable trigger)
  
