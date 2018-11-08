module QuickChecks where

import Test.QuickCheck
import Data.List (sort)
import Data.Char (toUpper)
import Ciphers

half x = x / 2

halfIdentity = (*2) . half

-- 1
prop_halfIsHalf :: Float -> Bool
prop_halfIsHalf x = x == halfIdentity x

-- 2
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

-- 3
plusAssociative :: Integer -> Integer -> Integer -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: Integer -> Integer -> Bool
plusCommutative x y = x + y == y + x

-- 4
multAssociative :: Int -> Int -> Int -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

multCommutative :: Int -> Int -> Bool
multCommutative x y = x * y == y * x

-- 5
-- can't divide by zero!
nonzeroIntGen :: Gen Int
nonzeroIntGen = suchThat arbitrary (/=0)

quotRemLaw :: Int -> Int -> Bool
quotRemLaw x y = (quot x y)*y + (rem x y) == x

divModLaw :: Int -> Int -> Bool
divModLaw x y = (div x y)*y + (mod x y) == x

prop_quotRem :: Property
prop_quotRem = forAll nonzeroIntGen (\x -> forAll nonzeroIntGen (quotRemLaw x))

prop_divMod :: Property
prop_divMod = forAll nonzeroIntGen (\x -> forAll nonzeroIntGen (divModLaw x))

-- 6
expAssoc :: Int -> Int -> Int -> Bool
expAssoc x y z = x ^ (y ^ z) == (x ^ y) ^ z

expComm :: Int -> Int -> Bool
expComm x y = x ^ y == y ^ x

-- 7
reverseIdentity :: [Int] -> Bool
reverseIdentity xs = xs == (reverse . reverse) xs

-- 8
prop_dollarSignIsApplication :: Int -> Bool
prop_dollarSignIsApplication x = id x == (id $ x)

-- 9
prop_concatsEqual :: [Int] -> [Int] -> Bool
prop_concatsEqual xs ys = foldr (:) xs ys == (++) xs ys

prop_concatnip :: [[Int]] -> Bool
prop_concatnip xs = foldr (++) [] xs == concat xs

-- 10
f :: Int -> [Int] -> Bool
f n xs = length (take n xs) == n

-- 11
readShow :: Int -> Bool
readShow x = (read (show x)) == x

-- idempotence

-- 1
twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = toUpper x:xs

prop_capIdempotent :: String -> Bool
prop_capIdempotent x =
  (capitalizeWord x == twice capitalizeWord x)
  &&
  (capitalizeWord x == fourTimes capitalizeWord x)

-- 2
prop_sortIdempotent :: [Int] -> Bool
prop_sortIdempotent xs =
  (sort xs == twice sort xs)
  &&
  (sort xs == fourTimes sort xs)

-- Make a Gen random generator for the datatype

-- 1
data Fool = Fulse | Frue deriving (Eq, Show)

foolGen :: Gen Fool
foolGen = elements [Fulse, Frue]

-- 2
unevenFoolGen :: Gen Fool
unevenFoolGen = frequency [(1, return Frue), (2, return Fulse)]

-- Validating ciphers

caesarIdentity :: Int -> String -> Bool
caesarIdentity p x = x == unCaesar p (caesar p x)

vigenereIdentity :: String -> String -> Bool
vigenereIdentity x k = x == unVigenere (vigenere x k) k

-- disallow empty string, because an empty key doesn't make sense
nonEmptyStrGen :: Gen String
nonEmptyStrGen = suchThat arbitrary (/= "")

prop_vigenere :: Property
prop_vigenere = forAll arbitrary (\msg -> forAll nonEmptyStrGen (\key -> vigenereIdentity msg key))

main :: IO ()
main = do
  quickCheck prop_halfIsHalf 
  quickCheck $ \xs -> listOrdered (sort (xs :: [Int]))
  quickCheck plusAssociative
  quickCheck plusCommutative
  quickCheck multAssociative
  quickCheck multCommutative
  quickCheck prop_quotRem
  quickCheck prop_divMod
  -- these two fail, cause laws of math
  quickCheck expAssoc
  quickCheck expComm
  --
  quickCheck reverseIdentity
  quickCheck prop_dollarSignIsApplication
  -- nope
  quickCheck prop_concatsEqual
  -- 
  quickCheck prop_concatnip
  -- empty list, you tricky lass!
  quickCheck f
  quickCheck readShow
  quickCheck prop_capIdempotent
  quickCheck prop_sortIdempotent
  quickCheck caesarIdentity
  quickCheck prop_vigenere