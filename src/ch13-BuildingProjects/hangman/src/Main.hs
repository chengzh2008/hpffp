module Main where

import WordFactory
import Engine

main :: IO ()
main = do
  gw <- randomWord'
  print $ "wording is: " ++ gw
  runGame $ freshPuzzle gw
