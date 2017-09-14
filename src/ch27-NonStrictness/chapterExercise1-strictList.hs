{-# LANGUAGE Strict #-}
module StrictList where

data List a = Nil | Cons a (List a) deriving (Eq, Show)

take' n _ | n <= 0 = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n -1) xs)

map' _ Nil = Nil
map' f (Cons x xs) = Cons (f x) (map' f xs)

repeat' x = xs where xs = Cons x xs

main = do
  -- it will halt as repeat' is indefinitely, but the Strict pragma is turned on.
  print $ take' 10 $ map' (+1) (repeat' 1)
