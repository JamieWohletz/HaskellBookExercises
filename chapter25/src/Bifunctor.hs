module Bifunctor where

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b)
        -> (c -> d)
        -> p a c
        -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap aToB cToD (Deux a c) =
    Deux (aToB a) (cToD c)

data Const a b = Const a

instance Bifunctor Const where
  bimap aToB cToD (Const a) =
    Const $ aToB a

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap aToB cToD (Drei naw a c) =
    Drei naw (aToB a) (cToD c)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap aToB cToD (SuperDrei naw a) =
    SuperDrei naw (aToB a)

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei naw) =
    SemiDrei naw

data Quadriceps a b c d =
  Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap aToB cToD (Quadzzz naw nope a c) =
    Quadzzz naw nope (aToB a) (cToD c)

-- renamed this to One to avoid conflicts with the Prelude
data One a b =
    ThisOne a
  | ThatOne b

instance Bifunctor One where
  bimap aToB _ (ThisOne a) = ThisOne $ aToB a
  bimap _ cToD (ThatOne c) = ThatOne $ cToD c
