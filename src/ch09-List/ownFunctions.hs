module OwnFunctions where

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = if not x then x else myAnd xs

myAnd' :: [Bool] -> Bool
myAnd' [] = True
myAnd' (x:xs) = x && myAnd' xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs
