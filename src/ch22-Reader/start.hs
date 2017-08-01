import Control.Applicative

boop = (*2)
doop = (+10)

-- function composition
bip :: Integer -> Integer
bip = boop . doop

-- context is Functor
bloop :: Integer -> Integer
bloop = fmap boop doop

-- context is Applicative
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

-- context is Monad
boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)
