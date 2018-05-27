-- #1

data Sum a b = First a | Second b
               deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

-- #2
-- We can't write a Functor instance for Sum or Either
-- that operates on the First or Left data constructor, respectively,
-- because Functor expects a type with the kind * -> *, which means
-- that we have to partially apply Sum and Either to write instances
-- for them. We can't partially apply a type constructor from the right,
-- so we can't write Functors that operate on the data
-- constructor which uses the first type parameter.
