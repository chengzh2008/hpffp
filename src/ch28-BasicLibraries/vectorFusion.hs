module Main where

import Criterion.Main
import qualified Data.Vector as V

testV' :: Int -> V.Vector Int
testV' n =
  V.map ((+n)) $ V.map ((+n)) $ V.map ((+n)) $ V.map ((+n)) $ V.fromList [1..10000]

testV :: Int -> V.Vector Int
testV n =
  V.map ((+n) . (+n) . (+n) . (+n)) $ V.fromList [1..10000]

main :: IO ()
main = defaultMain
  [ bench "vector map prefused" $ whnf testV 9998
  , bench "vector map will be fused" $ whnf testV' 9998
  ]


{-
benchmarking vector map prefused
time                 103.3 μs   (102.6 μs .. 104.1 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 103.7 μs   (103.3 μs .. 104.2 μs)
std dev              1.567 μs   (1.282 μs .. 2.189 μs)

benchmarking vector map will be fused
time                 103.0 μs   (102.5 μs .. 103.7 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 102.9 μs   (102.4 μs .. 103.4 μs)
std dev              1.609 μs   (1.342 μs .. 1.941 μs)
-}
