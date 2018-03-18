-- 1
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

-- 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee b f (Just a) = f a

-- 3
fromMaybe :: a -> Maybe a -> a
fromMaybe a = mayybee a id

-- 4
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- 5
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) =
  case x of
    (Just y) -> y:catMaybes xs
    Nothing -> catMaybes xs

-- 6
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs =
  case (any (not . isJust) xs) of
    True -> Nothing
    False -> Just (map (\(Just x) -> x) xs)
