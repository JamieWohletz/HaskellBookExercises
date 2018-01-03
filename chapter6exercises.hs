data TisAnInteger = TisAn Integer

-- Chapter exercises

-- Does it typecheck?
-- #1
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- #2
data Mood = Blah | Woot deriving (Show, Eq)

settleDown x = if x == Woot
	 	 then Blah
		 else x

-- #3
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

-- Given a datatype declaration, what can we do?
-- #1
data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)


