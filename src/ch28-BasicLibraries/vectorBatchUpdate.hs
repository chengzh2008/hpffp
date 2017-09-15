module Main where

import Criterion.Main
import Data.Vector ((//))
import qualified Data.Vector as V

vec :: V.Vector Int
vec = V.fromList [1..10000]

slow :: Int -> V.Vector Int
slow n = go n vec
  where go 0 v = v
        go n v = go (n - 1) (v // [(n, 0)])

batchList :: Int -> V.Vector Int
batchList n = vec // updates
  where updates = fmap (\n -> (n, 0)) [0..n]

main :: IO ()
main = defaultMain
  [ bench "slow" $ whnf slow 9998
  , bench "batch update list" $ whnf batchList 9998
  ]

{-
benchmarking slow
time                 137.1 ms   (134.8 ms .. 140.3 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 136.3 ms   (135.4 ms .. 137.6 ms)
std dev              1.583 ms   (919.0 μs .. 2.407 ms)
variance introduced by outliers: 11% (moderately inflated)

benchmarking batch update list
time                 271.1 μs   (242.2 μs .. 300.5 μs)
                     0.960 R²   (0.941 R² .. 1.000 R²)
mean                 249.1 μs   (243.2 μs .. 262.6 μs)
std dev              26.90 μs   (9.806 μs .. 47.37 μs)
variance introduced by outliers: 82% (severely inflated)


-}
