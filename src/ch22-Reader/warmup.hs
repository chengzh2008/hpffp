import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs


-- function composition
composed :: [Char] -> [Char]
composed = rev . cap

-- function is an instance of Monoid
combined :: [Char] -> [Char]
combined = cap `mappend` rev

-- function is an instance of Functor
-- for function, fmap is .
fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

-- function is an instance of Applicative
tuppled :: [Char] -> ([Char], [Char])
tuppled = (,) <$> cap <*> rev

-- function is an instance of Monad
tuppled' :: [Char] -> ([Char], [Char])
tuppled' = do
  a <- cap
  b <- rev
  return (a, b)
