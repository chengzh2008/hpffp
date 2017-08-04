module RandomExample where

import System.Random

data Die = DieOne
         | DieTwo
         | DieThree
         | DieFour
         | DieFive
         | DieSix
         deriving (Eq, Enum, Show)

intToDie :: Int -> Die
intToDie = toEnum . pred

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      range = (1, 6)
      (d1, s1) = randomR range s
      (d2, s2) = randomR range s1
      (d3, _) = randomR range s2
  (intToDie d1, intToDie d2, intToDie d3)
