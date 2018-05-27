-- 1

hello = const <$> Just "Hello" <*> pure "World"

-- 2

quatro = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]
