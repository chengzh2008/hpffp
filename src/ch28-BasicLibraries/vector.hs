module Main where

import Criterion.Main

import qualified Data.Vector as V

slice :: Int -> Int -> [a] -> [a]
slice from len = take len . drop from

l :: [Int]
l = [1..1000]

v :: V.Vector Int
v = V.fromList l

main :: IO ()
main = defaultMain
  [ bench "slicing list" $ whnf (head . slice 100 900) l
  , bench "slicing vector" $ whnf (V.head . V.slice 100 900) v
  ]

{-  result
benchmarking slicing list
time                 171.5 ns   (169.9 ns .. 173.6 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
  mean                 171.3 ns   (170.4 ns .. 172.3 ns)
  std dev              3.282 ns   (2.709 ns .. 3.850 ns)
  variance introduced by outliers: 25% (moderately inflated)

  benchmarking slicing vector
  time                 27.25 ns   (26.33 ns .. 28.00 ns)
                       0.994 R²   (0.991 R² .. 0.996 R²)
  mean                 26.73 ns   (26.03 ns .. 27.39 ns)
  std dev              2.293 ns   (1.937 ns .. 2.925 ns)
  variance introduced by outliers: 89% (severely inflated)
-}
