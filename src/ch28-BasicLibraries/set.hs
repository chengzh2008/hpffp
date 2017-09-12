module Main where

import Criterion.Main
import qualified Data.Map as M
import qualified Data.Set as S

bumpIt (i, v) = (i + 1, v + 1)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream
  where stream = iterate bumpIt (0, 0)

s :: S.Set Int
s = S.fromList $ take 10000 stream
  where stream = iterate (+1) 0

membersMap :: Int -> Bool
membersMap i = M.member i m

membersSet :: Int -> Bool
membersSet i = S.member i s

main :: IO ()
main = defaultMain
  [ bench "member check map" $ whnf membersMap 9999
  , bench "member check set" $ whnf membersSet 9999
  ]

-- stack ghc -- -O2 set.hs
-- ./set
{- result
benchmarking member check map
time                 46.17 ns   (45.52 ns .. 46.77 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 45.11 ns   (44.27 ns .. 45.94 ns)
std dev              2.743 ns   (2.417 ns .. 3.118 ns)
variance introduced by outliers: 79% (severely inflated)

benchmarking member check set
time                 43.23 ns   (42.97 ns .. 43.53 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 43.52 ns   (43.22 ns .. 44.02 ns)
std dev              1.226 ns   (740.0 ps .. 1.939 ns)
variance introduced by outliers: 45% (moderately inflated)
-}
