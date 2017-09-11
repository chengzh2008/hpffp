passiblyKaboom = \f -> f fst snd (0, undefined)

-- booleans in lambda for True
true :: a -> a -> a
true = \a -> (\b -> a)

false :: a -> a -> a
false = \a -> (\b -> b)

-- apply true to passiblyKaboom: the process of reduction

-- (\f -> f fst snd (0, undefined)) (\a -> (\b -> a))
-- (\a -> (\b -> a) fst snd (0, undefined))
-- (\b -> fst) snd (0, undefined)
-- fst (0, undefined)
-- 0

-- apply false to passiblyKaboom

-- (\f -> f fst snd (0, undefined)) (\a -> (\b -> b))
-- (\a -> (\b -> b)) fst snd (0, undefined)
-- (\b -> b) snd (0, undefined)
-- snd (0, undefined)
-- undefined   -- wound bottom out

data Test = A Test2 | B Test2 deriving (Show)
data Test2 = C Int | D Int deriving (Show)

forceNothing :: Test -> Int
forceNothing _ = 0

forceTest :: Test -> Int
forceTest (A _) = 1
forceTest (B _) = 2

forceTest2 :: Test -> Int
forceTest2 (A (C i)) = i
forceTest2 (A (D i)) = i
forceTest2 (B (C i)) = i
forceTest2 (B (D i)) = i
