import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0 = "Buzz"
           | n `mod` 3 == 0 = "Fizz"
           | otherwise = show n

-- TODO: compiler complains State is expecting one more argument.
addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList l = execState (mapM_ addResult l) []

main :: IO ()
main = do
  -- mapM_ (putStrLn . fizzBuzz) [1..100]
  mapM_ putStrLn $ reverse $ fizzBuzzList [1..100]
