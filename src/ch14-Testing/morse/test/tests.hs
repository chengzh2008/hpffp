module Main where

import qualified Data.Map as M
import Morse
import Test.QuickCheck

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
  forAll charGen
  (\c -> ((charToMorse c) >>= morseToChar) == Just c)

prop_thereAndBackAgain' :: Property
prop_thereAndBackAgain'=
  forAll morseGen
  (\m -> ((morseToChar m) >>= charToMorse) == Just m)

main :: IO ()
main = do
  quickCheck prop_thereAndBackAgain
  quickCheck prop_thereAndBackAgain'
