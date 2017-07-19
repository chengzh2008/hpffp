module EitherMonad where

type Founded = Int
type Coders = Int
data SoftwareShop = Shop { founded :: Founded
                         , programmers :: Coders } deriving (Eq, Show)

data FoundedError = NegativeYears Founded
                  | TooManyYears Founded
                  | NegativeCoders Coders
                  | TooManyCoders Coders
                  | TooManyCodersForYears Founded Coders deriving (Eq, Show)

validateFounded :: Founded -> Either FoundedError Founded
validateFounded n | n < 0 = Left $ NegativeYears n
                  | n > 500 = Left $ TooManyYears n
                  | otherwise = Right n

validateCoders :: Coders -> Either FoundedError Coders
validateCoders n | n < 0 = Left $ NegativeYears n
                 | n > 5000 = Left $ TooManyCoders n
                 | otherwise = Right n

mkSoftware :: Founded -> Coders -> Either FoundedError SoftwareShop
mkSoftware year coders = do
  founded <- validateFounded year
  programmers <- validateCoders coders
  if programmers > div founded 10
    then Left $ TooManyCodersForYears founded programmers
    else Right $ Shop founded programmers
