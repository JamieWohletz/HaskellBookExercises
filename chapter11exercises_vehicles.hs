data Price =
  Price Integer deriving (Eq, Show)

data Size = Small | Medium | Big deriving (Ord, Eq, Show) 
 
data Manufacturer =
  Mini 
  | Mazda
  | Tata
  deriving (Eq, Show)
 
data Airline =
  PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)
 
data Vehicle = Car Manufacturer Price
  | Plane Airline Size
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)
yourCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir Big

-- 1
-- Vehicle

-- 2
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3
getManu :: Vehicle -> Manufacturer
getManu (Car man _) = man 

-- 4
-- The getManu function will throw an unmatched pattern
-- error if passed a Plane. 

-- 5

