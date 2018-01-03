<<<<<<< HEAD
x = (+)
f xs = w `x` 1
  where w = length xs

id = \x -> x

foist = fst
=======
data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah

-- Correcting syntax
-- 1.
x = (+)
f :: String -> Int
f xs = w `x` 1
  where  w = length xs

-- 2. 
id' x = x

-- 3.
listFst :: [a] -> a
listFst (x:xs) = x

-- 4.
fst' (a, b) = a
>>>>>>> 679f21c... Chapter 11 + misc exercises
