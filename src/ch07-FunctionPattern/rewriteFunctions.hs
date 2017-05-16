{-# LANGUAGE NoMonomorphismRestriction #-}
module RewriteFuction where

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = (+1)

-- interesting observation without the above extension:
-- the inferenced type signature is
-- (Num a, Ord a) => a -> a -> a
addFive x y = (if x > y then y else x) + 5

-- interesting observation without the above extension:
-- the inferenced type signature is
-- Integer -> Integer -> Integer
addFive' = \x -> \y -> (if x > y then y else x) + 5
-- when the extension is used, both have the same type signature
-- (Num a, Ord a) => a -> a -> a

mflip f = \x -> \y -> f y x
mflip' f x y = f y x
