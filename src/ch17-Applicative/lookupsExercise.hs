import Data.List (elemIndex)

xs = [1, 2, 3]
ys = [4, 5, 6]
zs = [1..5]

-- 1:
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip xs ys)

-- 2:
y :: Maybe Integer
y = lookup 3 $ zip xs ys
z :: Maybe Integer
z = lookup 2 $ zip xs ys

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3:

a :: Maybe Int
a = elemIndex 3 zs

b :: Maybe Int
b = elemIndex 4 zs

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> a <*> b

-- 4:
summed :: Maybe Integer
summed = sum <$> ((,) <$> y <*> z)
