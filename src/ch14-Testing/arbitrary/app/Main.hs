module Main where

import ArbitraryTypeclass
import Test.QuickCheck

main :: IO ()
main = do
  sample trivialGen
