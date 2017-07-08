import Control.Applicative

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen then Nothing else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  case mkName n of
    Nothing -> Nothing
    Just n' ->
      case mkAddress a of
        Nothing -> Nothing
        Just a' ->
          Just $ Person n' a'

-- using Applicative
mkPerson' :: String -> String -> Maybe Person
mkPerson' n a =
  Person <$> mkName n <*> mkAddress a


-- another example Cow
data Cow = Cow { name :: String
               , age :: Int
               , weight :: Int
               } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty s = Just s

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

-- use Applicative
mkCow :: String -> Int -> Int -> Maybe Cow
mkCow name age weight = Cow <$> noEmpty name <*> noNegative age <*> noNegative weight

-- use liftA3
mkCow' :: String -> Int -> Int -> Maybe Cow
mkCow' name age weight = liftA3 Cow (noEmpty name) (noNegative age) (noNegative weight)


-- exercise: Fixer Upper

a = const <$> Just "Hello" <*> Just "World"
b = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tireness" <*> Just [1, 2, 3]
