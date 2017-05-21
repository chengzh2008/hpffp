module WordNumber where

import Data.List (intersperse)

data Digit = Zero | One | Two | Three | Four |
  Five | Six | Seven | Eight | Nine deriving (Eq, Show, Enum)

digitToWord :: Int -> String
digitToWord n
  | n < 0 || n > 10 = error "invalid digit"
  | otherwise = show (toEnum n :: Digit)

digits :: Int -> [Int]
digits n
  | n <= 9 = [n]
  | n > 9 = (digits $ quotient) ++ [remainder]
  where quotient = div n 10
        remainder = mod n 10
-- point free style
wordNumber :: Int -> String
wordNumber = foldr (++) "" . intersperse "-" . map digitToWord . digits
