module TupleFunctions where

-- partern match on tuple
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y= ((snd x, snd y), (fst x, fst y))

f' :: (a, b) -> (c, d) -> ((b, d), (a, c))
f' (a, b) (c, d) = ((b, d), (a, c))

addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

f'' :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f'' (a, _, c) (d, _, f) = ((a, d), (c, f))
