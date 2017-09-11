module Main where

import Criterion.Main

myList :: [Int]
myList = [1..9999]

main :: IO ()
main = defaultMain
  -- bench mark only the first data constructor :
  [ bench "whnf map list 9999" $ whnf (map (+1)) myList
  , bench "nf map list 9999" $ nf (map (+1)) myList
  ]
