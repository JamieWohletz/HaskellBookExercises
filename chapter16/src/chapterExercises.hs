{-# LANGUAGE FlexibleInstances #-}
-- Valid Functor instance possible?

-- 1
-- No

-- 2
-- Yes

data BoolAndSomethingElse a = 
  False' a | True' a

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' $ f a
  fmap f (True' a) = True' $ f a

-- 3
-- Yes

-- 4
-- No; the kind of Mu is (* -> *) -> *

-- 5
-- No

-- Rearrange type parameters

-- 1
data Sum b a = First a | Second b deriving (Eq, Show)

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

-- 2
data Company a c b =
    DeepBlue a c
  | Something b
  deriving (Eq, Show)

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3

data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More b) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Write Functor instances for the following datatypes.

-- 1
data Quant a b =
    Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor $ f b
  fmap _ Finance = Finance
  fmap f (Desk a) = Desk a

-- 2

data K' a b = K' a

instance Functor (K' a) where
  fmap f (K' a) = K' a

-- 3

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K a b = K a deriving Show

instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip (K (f b))

-- 4

data EvilGoateeConst a b = GoatyConst b deriving Show

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- 5

data LiftItOut f a = LiftItOut (f a) deriving Show

-- gimme dat Functor instance
instance Functor m => Functor (LiftItOut m) where
  fmap f (LiftItOut m) = LiftItOut $ f <$> m

-- 6

data Parappa f g a =
  DaWrappa (f a) (g a) deriving Show
-- note: in lines like the above, f and g can be
-- functions OR any other higher-kinded type.

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa m1 m2) = DaWrappa (f <$> m1) (f <$> m2)

-- 7

data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b) deriving Show

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething m m') = IgnoringSomething m (f <$> m')

-- 8

data Notorious g o a t =
  Notorious (g o) (g a) (g t)
  deriving Show

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious m1 m2 m3) = Notorious m1 m2 (f <$> m3)

-- 9

data List a = 
    Nil
  | Cons a (List a)
  deriving Show

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)

-- 10

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
  deriving Show

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats g1 g2 g3) = MoreGoats (f <$> g1) (f <$> g2) (f <$> g3)
-- look out, the goats are swarming!

-- 11

data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s $ f a
  fmap f (Read g) = Read $ f . g
