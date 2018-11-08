module DifferenceList where

newtype DList a = DL { unDL :: [a] -> [a] }

-- 1
empty :: DList a
empty = DL $ \_ -> []
{-# INLINE empty #-}

-- 2
singleton :: a -> DList a
singleton a = DL $ \_ -> [a]
{-# INLINE singleton #-}

-- 3
toList :: DList a -> [a]
toList (DL f) = f []
{-# INLINE toList #-}

-- 4
infixr `cons`
cons      :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)
{-# INLINE cons #-}

-- 5
infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL ((++[x]) . unDL xs)
{-# INLINE snoc #-}

-- 6
append :: DList a -> DList a -> DList a
append xs ys = DL $ \zs ->
                      let fys = unDL ys
                          fxs = unDL xs
                      in fxs zs ++ fys zs
{-# INLINE append #-}