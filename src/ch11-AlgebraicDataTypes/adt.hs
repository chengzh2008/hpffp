module AlgebraicDataType where

data Doggies a = Husky a | Mastiff a deriving (Eq, Show)

data Price = Price Integer deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plaine Airline Size
  deriving (Eq, Show)

myCar = Car Mini $ Price 14000
urcar = Car Mazda $ Price 20000
hsCar = Car Tata $ Price 11000
doge = Plaine PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plaine _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu
getManu (Plaine _ _) = error "not a car"
