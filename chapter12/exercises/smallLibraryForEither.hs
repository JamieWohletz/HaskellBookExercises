-- 1
lefts' :: [Either a b] -> [a]
lefts' = foldr folder []
  where
    folder :: Either a b -> [a] -> [a]
    folder e xs =
      case e of
        (Right _) -> xs
        (Left a) -> a:xs

-- 2
rights' :: [Either a b] -> [b]
rights' = foldr folder []
  where
    folder :: Either a b -> [b] -> [b]
    folder e xs =
      case e of
        (Left _) -> xs
        (Right b) -> b:xs

-- 3
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' es = (lefts' es, rights' es)

-- 4
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just $ f b

-- 5
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ f (Right b) = f b

-- 6
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) (Just . f) 
