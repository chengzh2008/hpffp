module MonadExercise where

import Control.Monad (join)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

bind :: Monad m => (a -> m b) -> m a -> m b
bind f fa = join $ fmap f fa


-- List Monad
twiceWhenEven :: [Int] -> [Int]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []

-- implement monad instance for Sum a b
data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

instance (Monoid a) => Applicative (Sum a) where
  pure a = Second a
  (First a) <*> (First b) = First (a `mappend` b)
  (Second fb) <*> (Second b) = Second $ fb b
  _ <*> (First a) = First a
  (First a) <*> _ = First a


instance (Monoid a) => Monad (Sum a) where
  return = pure
  (First a) >>= _ = First a
  (Second b) >>= f = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency $ [ (1, return $ First a)
                , (1, return $ Second b)]
instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ monad (undefined :: Sum String (String, Maybe String, [Int]))
