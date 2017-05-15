module FunctFromType where

myFunc :: (x -> y)
       -> (y -> z)
       -> c
       -> (a, x)
       -> (a, z)
myFunc xToY yToZ _ (a, x) =
  (a, yToZ . xToY $ x)


i :: a -> a
i = id

c :: a -> b -> a
c x _ = x

c' :: a -> b -> b
c' _ y = y

co :: (b -> c) -> (a -> b) -> a -> c
co f g = f . g

a :: (a -> c) -> a -> a
a _ a = a

a' :: (a -> b) -> a -> b
a' f a = f a
