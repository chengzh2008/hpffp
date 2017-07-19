module BadMonad where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data CountMe a = CountMe Integer a deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe n a) = CountMe n $ f a

instance Applicative CountMe where
  pure a = CountMe 0 a
  (CountMe n fa) <*> (CountMe n' a) = CountMe (n + n') $ fa a

instance Monad CountMe where
  return = pure
  -- monad needs to have a consistent behavior for applicative in order to be a valid monad
  CountMe n a >>= f = let (CountMe n' a') = f a in CountMe (n + n') a'

instance (Arbitrary a) => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance (Eq a) => EqProp (CountMe a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ functor (undefined :: CountMe (String, Maybe String, [Int]))
  quickBatch $ applicative (undefined :: CountMe (String, Maybe String, [Int]))
  quickBatch $ monad (undefined :: CountMe (String, Maybe String, [Int]))
