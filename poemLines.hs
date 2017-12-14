module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
            \ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines str = breakAt '\n' str

-- New function
breakAt :: Char -> String -> [String]
breakAt ch string = break ch string []
  where
    break :: Char -> String -> [String] -> [String]
    break _ "" strings = strings
    break ch s strings = break ch dropped $ strings ++ [taken]
      where
        dropped = dropWhile (== ch) $ dropWhile (/= ch) s
        taken = takeWhile (/= ch) s

shouldEqual = 
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $
  "Are they equal? "
  ++ show (myLines sentences == shouldEqual)
