module Chapter3Exercises where

-- #2

bang :: String -> String
bang str = str ++ "!"

-- assuming they meant to write 'y' instead of "y"
fifth :: String -> Char
fifth str = str !! 4

drop9 :: String -> String
drop9 = drop 9

-- #3

thirdLetter :: String -> Char
thirdLetter str = str !! 2

-- #4
testString :: String
testString = "Scooby dooby doo!"

-- 1-indexed
letterIndex :: Int -> Char
letterIndex i = testString !! (i - 1)

-- #5
rvrs :: String -> String
rvrs str = thirdWord ++ " " ++ secondWord ++ " " ++ firstWord
  where
    firstWord = take 5 str
    secondWord = take 2 $ drop 6 str
    thirdWord = take 7 $ drop 9 str
