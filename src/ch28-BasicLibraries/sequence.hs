module Main where

import Criterion.Main
import qualified Data.Sequence as S

lists :: [[Int]]
lists = replicate 10 [1..100000]

seqs :: [S.Seq Int]
seqs = replicate 10 (S.fromList [1..100000])

main :: IO ()
main = defaultMain
  [ bench "concatenate lists" $ nf mconcat lists
  , bench "concatenate sequences" $ nf mconcat seqs
  ]

-- stack ghc -- -O2 sequence.hs
-- ./sequence

{- result
benchmarking concatenate lists
time                 10.96 ms   (10.59 ms .. 11.46 ms)
                     0.991 R²   (0.985 R² .. 0.997 R²)
mean                 10.78 ms   (10.61 ms .. 10.99 ms)
std dev              479.5 μs   (386.6 μs .. 588.2 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking concatenate sequences
time                 6.757 ms   (6.584 ms .. 6.993 ms)
                     0.994 R²   (0.991 R² .. 0.997 R²)
mean                 6.694 ms   (6.603 ms .. 6.785 ms)
std dev              271.1 μs   (240.5 μs .. 313.7 μs)
variance introduced by outliers: 18% (moderately inflated)
-}
