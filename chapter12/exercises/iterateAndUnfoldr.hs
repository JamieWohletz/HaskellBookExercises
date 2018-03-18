-- 1
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x:myIterate f (f x)

-- 2
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b =
  case f b of
    Nothing -> []
    Just (a, b2) -> a:myUnfoldr f b2

-- 3
betterIterate :: (a -> a) -> a -> [a]
betterIterate f a = myUnfoldr (\b -> Just (b, f b)) a
