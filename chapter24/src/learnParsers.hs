module LearnParsers where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1'

one' = one >> stop

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

-- 1
oneEof :: Parser Char
oneEof = one <* eof

oneTwoEof :: Parser Char
oneTwoEof = oneTwo <* eof

-- 2
-- I guess? who knows.
strParser :: String -> Parser String
strParser s = string s

-- 3
string' :: String -> Parser String
string' = mapM char
-------------------

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

pNL s =
  putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one': "
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'