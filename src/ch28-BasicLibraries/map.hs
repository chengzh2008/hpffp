module Main where

import Criterion.Main
import qualified Data.Map as M

genList :: Int -> [(String, Int)]
genList n = go n []
  where go 0 xs = ("0", 0) : xs
        go n' xs = go (n' - 1) ((show n', n') : xs)

pairList :: [(String, Int)]
pairList = genList 9001

testMap :: M.Map String Int
testMap = M.fromList pairList

main :: IO ()
main = defaultMain
  [ bench "lookup one thing, list" $ whnf (lookup "doesntExist") pairList
  , bench "lookup one thing, map" $ whnf (M.lookup "doesntExist") testMap
  ]

-- stack build criterion
-- stack ghc -- -O2 map.hs
-- ./map
{- result
benchmarking lookup one thing, list
time                 123.6 μs   (122.6 μs .. 124.5 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 124.0 μs   (123.1 μs .. 126.1 μs)
std dev              4.443 μs   (2.356 μs .. 8.576 μs)
variance introduced by outliers: 35% (moderately inflated)

benchmarking lookup one thing, map
time                 139.2 ns   (133.7 ns .. 143.3 ns)
                     0.994 R²   (0.993 R² .. 0.997 R²)
mean                 133.8 ns   (131.8 ns .. 136.5 ns)
std dev              7.854 ns   (6.412 ns .. 9.887 ns)
variance introduced by outliers: 77% (severely inflated)
-}
