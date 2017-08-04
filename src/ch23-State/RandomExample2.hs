module RandomExample2 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
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

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes :: State StdGen (Die, Die, Die)
rollDieThreeTimes = liftA3 (,,) rollDie' rollDie' rollDie'

-- take 6 $ evalState infiniteDie (mkStdGen 0)
-- this is just repeating the Die itself, not the state action that produce the die
infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie'

-- this is what we need
nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie'



-- keep rolling until getting to the limit
go :: Int -> Int -> Int -> StdGen -> Int
go limit sum count gen
  | sum >= limit = count
  | otherwise =
    let (die, nextGen) = randomR (1, 6) gen
    in go limit (sum + die) (count + 1) nextGen

-- keep rolling until 20
-- example:  (rollsToGetTwenty . mkStdGen) <$> randomIO
rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 20 0 0 g

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go n 0 0 g

go1 :: Int -> Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
go1 limit sum re gen
  | sum >= limit = re
  | otherwise =
    let (die, nextGen) = randomR (1, 6) gen
    in go1 limit (sum + die) (fst re + 1, snd re ++ [intToDie die]) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go1 n 0 (0, []) g
