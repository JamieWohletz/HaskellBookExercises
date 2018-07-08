-- 1

data Constant a b =
  Constant b

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b

-- 2

data Two a b =
  Two a b

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

-- 3

data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

-- 4

data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ b1 b2) = f b1 `mappend` f b2

-- 5

data Four' a b =
  Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' _ b1 b2 b3) = f b1 `mappend` f b2 `mappend` f b3

-- 6

filterF :: (Applicative f
           , Foldable t
           , Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF p t = foldMap (\a -> if p a then pure a else mempty) t
