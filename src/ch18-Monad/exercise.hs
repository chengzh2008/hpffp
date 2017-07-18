module MonadExercise where

import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f fa = join $ fmap f fa


-- List Monad
twiceWhenEven :: [Int] -> [Int]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []


-- Maybe Monad
