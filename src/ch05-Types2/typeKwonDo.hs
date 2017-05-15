module TypeKwonDo where

data Woot
data Blah

f :: Woot -> Blah
f = undefined

g :: (Blah, Woot) -> (Blah, Blah)
g (x, y) = (x, f y)


f1 :: Int -> String
f1 = undefined

g1 :: String -> Char
g1 = undefined

h :: Int -> Char
h = g1 . f1

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f g = fst . g . f 
