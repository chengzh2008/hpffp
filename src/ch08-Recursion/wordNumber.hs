module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

digits :: Int -> [Int]
digits n
  | n <= 9 = [n]
  | n > 9 = (digits $ quotient) ++ [remainder]
  where quotient = div n 10
        remainder = mod n 10
-- point free style
wordNumber :: Int -> String
wordNumber = foldr (++) "" . intersperse "-" . map digitToWord . digits
