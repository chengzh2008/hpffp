module MatchTheTypes where

import Data.List (sort)

i :: Num a => a
i = 1

f :: Fractional a => a 
f = 1.0

g :: RealFrac a => a
g = 1.0

freud :: Ord a => a -> a
freud x = x

freud' :: Int -> Int 
freud' x = x

myX = 1 :: Int
sigmund :: Int -> Int
sigmund x = myX

jung :: Ord a => [a] -> a
jung xs = head (sort xs)

jung' :: [Int] -> Int 
jung' xs = head (sort xs)

young :: [Char] -> Char
young xs = head (sort xs)
  
young' :: Ord a => [a] -> a 
young' xs = head (sort xs)

mysort :: [Char] -> [Char]
mysort = sort

signifier :: [Char] -> Char
signifier xs = head (mysort xs)

mysort' :: [Char] -> [Char]
mysort' = sort

-- won't type checked
signifier' :: Ord a => [a] -> a
signifier' xs = head (mysort' xs)
