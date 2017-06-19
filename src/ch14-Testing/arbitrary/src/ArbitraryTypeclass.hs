module ArbitraryTypeclass where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)
import Control.Monad (liftM)

-- baby Arbitrary
data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen

-- identity Crisis
data Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return $ Identity a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

-- Arbitrary Products
data Pair a b = Pair a b deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return $ Pair a b

-- this is naccessary if the datatype is embedded in other data types
instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = pairGen

-- one instance of pairGen
pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

-- Arbitrary sums
data Sum a b =
  First a | Second b deriving (Eq, Show)

-- equal odds for each
sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a, return $ Second b]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

-- 10 times First a then Second b
sumGenFirstPls :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenFirstPls = do
  a <- arbitrary
  b <- arbitrary
  frequency [(10, return $ First a), (1, return $ Second b)]
{- or using liftM
sumGenFirstPls  = do
  frequency [(10, liftM First arbitrary),
             (1, liftM Second arbitrary)]
-}
