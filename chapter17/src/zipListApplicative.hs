import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import ListApplicative

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' = let (ZipList' l) = xs
            in take' 3000 l
      ys' = let (ZipList' l) = ys
            in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' $ Cons a Nil
  (ZipList' Nil) <*> _ = ZipList' Nil
  _ <*> (ZipList' Nil) = ZipList' Nil
  (ZipList' fs) <*> (ZipList' xs) =
    ZipList' $ fold (\(f, x) xs' -> Cons (f x) xs') Nil (zippy fs xs)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = arbitrary >>= pure . pure

verifyZipListApplicative :: IO ()
verifyZipListApplicative = quickBatch (applicative (ZipList' (Cons ("one", "two", "three") Nil)))
