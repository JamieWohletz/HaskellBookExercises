type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Enter your name:"
  name <- getLine
  putStrLn "Enter your age:"
  ageStr <- getLine
  let age = read ageStr :: Age
  mkPerson' name age

mkPerson' :: Name -> Age -> IO ()
mkPerson' name age =
  case mkPerson name age of
    (Left error) -> do
      putStrLn $ "Couldn't save that person. Reason: " ++ show error
    (Right person) -> do
      putStrLn "Yay! Successfully got a person: "
      putStrLn $ show person
