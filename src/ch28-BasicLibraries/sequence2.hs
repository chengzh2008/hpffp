module Main where

import Criterion.Main
import qualified Data.Sequence as S

lists :: [Int]
lists = [1..100000]

seqs :: S.Seq Int
seqs = S.fromList [1..100000]

main :: IO ()
main = defaultMain
  [ bench "indexing list" $ whnf (\xs -> xs !! 9001) lists
  , bench "indexing sequence" $ whnf (flip S.index 9001) seqs
  ]

{-
benchmarking indexing list
time                 23.53 μs   (22.75 μs .. 24.36 μs)
                     0.988 R²   (0.981 R² .. 0.994 R²)
mean                 23.64 μs   (22.86 μs .. 24.55 μs)
std dev              2.763 μs   (2.331 μs .. 3.251 μs)
variance introduced by outliers: 88% (severely inflated)

benchmarking indexing sequence
time                 137.1 ns   (135.4 ns .. 138.6 ns)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 136.6 ns   (135.2 ns .. 138.1 ns)
std dev              4.649 ns   (3.891 ns .. 5.481 ns)
variance introduced by outliers: 52% (severely inflated)
-}
