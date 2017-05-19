module HighOrderFunc where

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x
flip' f = \x y -> f y x

returnLast :: a -> b -> c -> d -> d
returnLast _ _ _ d = d

-- won't work
returnLast' :: (((a -> b) -> c) -> d) -> d
returnLast' _ _ _ d = d

-- this works
returnLast'' :: a -> (b -> (c -> (d -> d)))
returnLast'' _ _ _ d = d
