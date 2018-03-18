data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada a = a
  mappend a Nada = a
  mappend (Only x) (Only y) = Only $ x `mappend` y
