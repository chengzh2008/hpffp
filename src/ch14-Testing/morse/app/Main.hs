module Main where

import Control.Monad (forever, when)
import Data.List (intercalate)
import Data.Traversable (traverse)
import Morse (stringToMorse, morseToChar)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hGetLine, hIsEOF, stdin)

convertToMorse :: IO ()
convertToMorse = forever $ do
  weAreDone <- hIsEOF stdin
  when weAreDone exitSuccess

  line <- hGetLine stdin
  convertLine line
  where
    convertLine line = do
      case stringToMorse line of
        (Just str) -> putStrLn $ intercalate " " str
        Nothing -> do
          putStrLn $ "Error: " ++ line
          exitFailure

convertFromMorse :: IO ()
convertFromMorse = forever $ do
  weAreDone <- hIsEOF stdin
  when weAreDone exitSuccess

  line <- hGetLine stdin
  convertLine line
  where
    convertLine line = do
      let decode :: Maybe String
          decode = traverse morseToChar (words line)
      case decode of
        (Just s) -> putStrLn s
        Nothing -> do
          putStrLn $ "Error: " ++ line
          exitFailure

main :: IO ()
main = do
  mode <- getArgs
  case mode of
    [arg] ->
      case arg of
        "from" -> convertFromMorse
        "to" -> convertToMorse
        _ -> argError
    _ -> argError
  where
    argError = do
      putStrLn "Please specify the\
                \ first argument\
                \ as being 'from' or\
                \ 'to' morse,\
                \ such as: morse to"
      exitFailure