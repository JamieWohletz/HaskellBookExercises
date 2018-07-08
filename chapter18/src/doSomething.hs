doSomething = do
  a <- Nothing
  b <- (Just 6)
  c <- (Just 7)
  pure (a, b, c)

appDoSomething = 
  pure (,,) <*> Nothing <*> (Just 6) <*> (Just 7)

doSomething' n = do
  a <- f n
  b <- g a
  c <- h b
  pure (a, b, c)

-- Doesn't work, because you can't pass the value of one Applicative
-- operation to another one.
-- appDoSomething' n =
--   pure (,,) <*> f n <*> g a <*> h b

monadDoSomething' n =
  f n >>= \a -> g a >>= \b -> h b >>= \c -> pure (a, b, c)

f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer
g i =
  if even i
  then Just (i + 1)
  else Nothing

h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)
