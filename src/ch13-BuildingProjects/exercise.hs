module Exercise where

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age


gimmePerson :: IO ()
gimmePerson = do
  putStr "Please input your name: "
  name <- getLine
  putStr "\nPlease input your age: "
  age <- getLine
  case mkPerson name (read age :: Integer) of
    Left l -> print l
    Right r -> do
      putStrLn $ "Yay! successfully got a person: " ++ show r
