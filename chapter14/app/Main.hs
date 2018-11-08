module Main where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise =
             go (n - d) d (count + 1)

mult :: (Eq a, Num a) => a -> a -> a
mult _ 0 = 0
mult x n = x + mult x (n - 1)

main :: IO ()
main = hspec $ do
  describe "Arithmetic" $ do
    describe "Addition" $ do
      it "gives a number > 1 for 1 + 1" $ do
        (1 + 1) > 1 `shouldBe` True
      it "gives 4 for 2 + 2" $ do
        2 + 2 `shouldBe` 4
      it "x + 1 is always greater than x" $ do
        property $ \x -> x + 1 > (x :: Int)

    describe "dividedBy" $ do
      it "divides 15 by 3 to give 5" $ do
        dividedBy 15 3 `shouldBe` (5, 0)
      it "divides 22 by 5 to give 4R2" $ do
        dividedBy 22 5 `shouldBe` (4, 2)

    describe "mult" $ do
      it "multiplies 5 by 3 to give 15" $ do
        mult 5 3 `shouldBe` 15
      it "multiplies 100 by 30 to give 3000" $ do
        mult 100 30 `shouldBe` 3000
