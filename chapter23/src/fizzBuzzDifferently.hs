import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0  = "Buzz"
           | n `mod` 3 == 0  = "Fizz"
           | otherwise       = show n

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo lo hi = execState finalState []
  where
    finalState :: State [String] ()
    finalState = foldr toState (return ()) [lo..hi]

    toState :: Integer -> State [String] () -> State [String] ()
    toState n s = s >> modify (\xs -> fizzBuzz n:xs)