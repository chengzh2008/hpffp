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

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs)
  | a == x = True
  | otherwise = myElem a xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' a = myAny (a == )

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish = foldr (\a acc -> a ++ acc) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = concat . map f

squish' :: [[a]] -> [a]
squish' = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr f' (head xs) xs
  where f' a b
          | f a b == GT = a
          | otherwise = b

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = myMaximumBy (flip f)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
