module Main where

import Control.Monad.Primitive
import Control.Monad.ST
import Criterion.Main
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic.Mutable as GM

mutableUpdateIO :: Int -> IO (MV.MVector RealWorld Int)
mutableUpdateIO n = do
  mvec <- GM.new (n+1)
  go n mvec
  where go 0 v = return v
        go n v = (MV.write v n 0) >> go (n - 1) v

mutableUpdateST :: Int -> V.Vector Int
mutableUpdateST n = runST $ do
  mvec <- GM.new (n + 1)
  go n mvec
  where go 0 v = V.freeze v
        go n v = (MV.write v n 0) >> go (n - 1) v

main :: IO ()
main = defaultMain
  [ bench "mutable IO vector" $ whnfIO (mutableUpdateIO 9998)
  , bench "mutable ST vector" $ whnf mutableUpdateST 9998
  ]

-- stack ghc -- -O2 vectorMutable.hs
-- ./vectorMutable
{-
benchmarking mutable IO vector
time                 22.68 μs   (21.77 μs .. 23.58 μs)
                     0.992 R²   (0.990 R² .. 0.996 R²)
mean                 22.01 μs   (21.47 μs .. 22.59 μs)
std dev              1.918 μs   (1.600 μs .. 2.420 μs)
variance introduced by outliers: 81% (severely inflated)

benchmarking mutable ST vector
time                 35.27 μs   (34.31 μs .. 36.15 μs)
                     0.996 R²   (0.994 R² .. 0.998 R²)
mean                 35.10 μs   (34.52 μs .. 35.83 μs)
std dev              2.222 μs   (1.905 μs .. 2.660 μs)
variance introduced by outliers: 68% (severely inflated)

-}
