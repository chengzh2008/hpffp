module Enums where

-- write enumFromTo for types below

eft :: (Enum a, Ord a) => a -> a -> [a]
eft a1 a2
 | compare a1 a2 == GT = []
 | compare a1 a2 == EQ = [a1]
 | otherwise = a1 : eft (succ a1) a2

eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd  = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft
