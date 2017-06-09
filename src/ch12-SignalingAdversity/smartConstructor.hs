module SmartConstructor where

import Control.Applicative

type Name = String
type Age = Integer

data Person = Person Name Age deriving (Eq, Show)

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
  | name /= "" && age >= 0 = Just $ Person name age
  | otherwise = Nothing


data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)

-- it does not let u express a list of errors
mkPerson' :: Name -> Age -> Either PersonInvalid Person
mkPerson' name age
  | name /= "" && age >= 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | otherwise = Left AgeTooLow



-- this will have a list of errors
-- which is an applicative
type ValidatePerson a = Either [PersonInvalid] a

ageOkay :: Age -> ValidatePerson Age
ageOkay age
  | age >= 0 = Right age
  | otherwise = Left [AgeTooLow]

nameOkay :: Name -> ValidatePerson Name
nameOkay name
  | name /= "" = Right name
  | otherwise = Left [NameEmpty]

validate :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
validate (Right name) (Right age) = Right $ Person name age
validate (Left badName) (Left badAge) = Left (badName ++ badAge)
validate (Left badName) _ = Left badName
validate _ (Left badAge) = Left badAge

mkPerson'' :: Name -> Age -> ValidatePerson Person
mkPerson'' name age =
  validate (nameOkay name) (ageOkay age)

-- after learn applicative, this can be rewritten
mkPerson''' :: Name -> Age -> ValidatePerson Person
mkPerson''' name age =
  liftA2 Person (nameOkay name) (ageOkay age)
