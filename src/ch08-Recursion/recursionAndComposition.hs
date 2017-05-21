module RecursionAndComposition where

-- my own funciton composition
(./) :: (b -> c) -> (a -> b) -> a -> c
(./) f g = \x -> f $ g x

inc :: Num a => a -> a
inc = (+1)

three = inc ./ inc ./ inc $ 0

-- control the number of recursions explicitly
incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times - 1) n)

-- abstract the recursion out of the incTimes
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f $ applyTimes (n - 1) f b

-- abstract the recursion out of the incTimes
-- composition more obvious: point free style
applyTimes' :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes' 0 f = id
applyTimes' n f = f . applyTimes (n - 1) f

-- thus, rewrite incTime using applyTimes
incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes times inc n

--evaluation process:
{--
applyTimes 5 (+1) 5
= (+1) $ applyTimes 4 (+1) 5
= (+1) $ (+1) $ applyTimes 3 (+1) 5
= (+1) $ (+1) $ (+1) $ applyTimes 2 (+1) 5
= (+1) $ (+1) $ (+1) $ (+1) $ applyTimes 1 (+1) 5
= (+1) $ (+1) $ (+1) $ (+1) $ (+1) $ applyTimes 0 (+1) 5
= (+1) $ (+1) $ (+1) $ (+1) $ (+1) $ 5
= (+1) $ (+1) $ (+1) $ (+1) $ 6
= (+1) $ (+1) $ (+1) $ 7
= (+1) $ (+1) $ 8
= (+1) $ 9
= 10
--}
