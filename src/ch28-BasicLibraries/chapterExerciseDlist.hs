import Criterion.Main

newtype DList a = DL { unDL :: [a] -> [a] }

empty :: DList a
empty = DL $ \_ -> []

singleton :: a -> DList a
singleton a = DL $ (a:) . unDL empty

toList :: DList a -> [a]
toList = flip unDL []

infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL $ (x:) . unDL xs

-- TODO: understand why this is O(1)
infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL $ (unDL xs) . (x:)

append :: DList a -> DList a -> DList a
append xs ys = DL $ (unDL xs) . (unDL ys)

-- toList empty
-- []

-- toList $ singleton 3
-- [3]

-- toList $ cons 3 empty
-- [3]

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n - 1) ([n] ++ xs)

constructDList :: Int -> [Int]
constructDList i = toList $ go i empty
  where go 0 xs = xs
        go n xs = go (n - 1) (singleton n `append` xs)

main :: IO ()
main = defaultMain
  [ bench "concat list" $ whnf schlemiel 123456
  , bench "concat dlist" $ whnf constructDList 123456
  ]

-- stack ghc -- -O2 chapterExerciseDlist.hs
-- ./chapterExerciseDlist

{- result
08:17 $ ./chapterExerciseDlist
benchmarking concat list
time                 6.161 ms   (6.101 ms .. 6.225 ms)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 6.312 ms   (6.229 ms .. 6.511 ms)
std dev              350.0 μs   (109.7 μs .. 632.2 μs)
variance introduced by outliers: 31% (moderately inflated)

benchmarking concat dlist
time                 97.36 μs   (96.96 μs .. 97.86 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 97.34 μs   (97.07 μs .. 97.92 μs)
std dev              1.220 μs   (729.0 ns .. 2.065 μs)
-}
