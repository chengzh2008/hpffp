module MoreApplicative where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation err a = Failure' err | Success' a deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure' err) = Failure' err
  fmap f (Success' a) = Success' $ f a

instance Monoid e => Applicative (Validation e) where
  pure = Success'
  Failure' err <*> Success' a = Failure' err
  Success' a <*> Failure' err = Failure' err
  Success' f <*> Success' a = Success' $ f a
  Failure' err1 <*> Failure' err2 = Failure' $ err1 `mappend` err2

instance (Eq err, Eq a) => EqProp (Validation err a) where
  (=-=) = eq

instance Arbitrary Errors where
  arbitrary  = do
    frequency [ (1, return DividedByZero)
              , (1, return StackOverflow)
              , (1, return MooglesChewedWires)]

instance (Arbitrary err, Arbitrary a) => Arbitrary (Validation err a) where
  arbitrary = do
    err <- arbitrary
    a <- arbitrary
    frequency [ (1, return $ Failure' err)
              , (1, return $ Success' a)]


main :: IO ()
main = do
  quickBatch $ applicative (undefined :: Validation [Errors] (String, Maybe String, Sum Int))
