module UnitOfSuccess where

import Text.Trifecta

original :: Parser ()
original = integer >> eof

new = integer <* eof

test = do
  print $ parseString new mempty "123"
  print $ parseString new mempty "123abc"