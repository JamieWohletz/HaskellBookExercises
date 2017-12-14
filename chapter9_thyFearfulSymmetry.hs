module Chapter9ThyFearfulSymmetry where

import PoemLines (breakAt)
-- #1
-- Assuming the "wallfish" part in the book is a mistake.
myWords :: String -> [String]
myWords str = breakAt ' ' str

-- #2, #3
-- See poemLines.hs in this directory.
