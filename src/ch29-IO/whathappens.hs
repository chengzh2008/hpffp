module WhatHappens where

import Control.Concurrent

-- myData is a recipe to take an empty MVar
myData :: IO (MVar Int)
myData = newEmptyMVar

main :: IO ()
main = do
  mv <- myData
  putMVar mv 0
  -- this will work
  takeMVar mv >>= print
  --
  mv' <- myData
  -- this will work
  putMVar mv' 3
  --
  three <- takeMVar mv'
  print three

-- thread will block
{-
 λ:-) main
*** Exception: thread blocked indefinitely in an MVar operation
-}
