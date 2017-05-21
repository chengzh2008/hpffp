module Bottom where

f :: Bool -> Int
f True = error "Meh.."
f False = 0

f' :: Bool -> Maybe Int
f' False = Just 0
f' _ = Nothing

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = (+) (fibonacci $ n - 1) (fibonacci $ n - 1)

-- integral division
type Numerator = Integer
type Denominator = Integer
type Quotient = Integer
type Remainder = Integer

devideBy :: Numerator -> Denominator -> (Quotient, Remainder)
devideBy  num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)
