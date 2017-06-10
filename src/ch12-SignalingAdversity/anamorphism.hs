module Anamorphism where

import Data.List


mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
  where go :: Num a => a -> [a] -> a
        go n [] = n
        go n (x:xs) = go (n+x) xs

niceSum :: Num a => [a] -> a
niceSum = foldl' (+) 0

mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
  where go :: Num a => a -> [a] -> a
        go n [] = n
        go n (x:xs) = go (n*x) xs

niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1

mehConcat :: [[a]] -> [a]
mehConcat xs = go [] xs
  where go :: [a] -> [[a]] ->[a]
        go as [] = as
        go as (bs:bss) = go (as ++ bs) bss

niceConcat :: [[a]] -> [a]
niceConcat = foldl' (++) []

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b =
  case (f b) of
    Nothing -> []
    Just (m, n) -> m : myUnfoldr f n

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\a -> Just (a, f a))


-- unfold a tree
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a =
  case (f a) of
    Nothing -> Leaf
    Just (l, m, r) -> Node (unfold f l) m (unfold f r)

-- build a tree
treeBuild :: Integer -> BinaryTree Integer
treeBuild = unfold go
  where go :: Integer -> Maybe (Integer, Integer, Integer)
        go 0 = Nothing
        go m = Just (m - 1, m, m - 1)
