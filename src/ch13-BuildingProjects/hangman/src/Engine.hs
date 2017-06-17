module Engine where

import Control.Monad (forever)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import Data.Maybe (isJust)

-- String: the word we are trying to guess
-- [Maybe Char] the characters we filled so far
-- [Char] the letters we've guessed so far
data Puzzle = Puzzle String [Maybe Char] [Char]

maximumAllowedGuess :: Int
maximumAllowedGuess = 7

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar maybeC = case maybeC of
  Nothing -> '_'
  Just c -> c

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (fmap (\_ -> Nothing) s) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) c = elem c s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = elem c guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle gw discovered guessed) c =
  Puzzle gw discovered' guessed'
  where guessed' = if elem c guessed then guessed else c:guessed
        discovered' = map go $ zipWith (\a b -> (a, b)) gw discovered
        go :: (Char, Maybe Char) -> Maybe Char
        go (c1, maybeC)
          | c1 == c && maybeC == Nothing = Just c1
          | otherwise = maybeC

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guessedC = do
  putStrLn $ "Your guess was: " ++ [guessedC]
  case (charInWord puzzle guessedC, alreadyGuessed puzzle guessedC) of
    (_, True) -> do
      putStrLn "You already guessed that char, pick someting else"
      return puzzle
    (True, _) -> do
      putStrLn "This char is in the word, fill in the word accordingly"
      return $ fillInCharacter puzzle guessedC
    (False, _) -> do
      putStrLn "This char is not in the word, please guess again."
      return $ fillInCharacter puzzle guessedC

gameOver :: Puzzle -> IO ()
gameOver (Puzzle gw _ guessed) = do
  if (length guessed) > maximumAllowedGuess then
    do
      putStrLn "You lose!"
      putStrLn $ "The word was: " ++ gw
      exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character"
