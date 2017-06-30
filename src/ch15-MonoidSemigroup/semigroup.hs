{-# LANGUAGE DeriveGeneric #-}

module SemiGroup where

import GHC.Generics
import Data.Semigroup
import Test.QuickCheck hiding (Failure, Success)
import Control.Monad

-- Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- Identity
newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentityStringAssoc = Identity String -> Identity String -> Identity String -> Bool

-- product of two
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoStringIntAssoc = Two String (Sum Int) -> Two String (Sum Int) -> Two String (Sum Int) -> Bool


-- product of three
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three d e f) = Three (a <> d) (b <> e) (c <> f)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeStringIntIntAssoc = Three String (Sum Int) (Product Int) -> Three String (Sum Int) (Product Int) -> Three String (Sum Int) (Product Int) -> Bool

-- product of three
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four e f g h) = Four (a <> e) (b <> f) (c <> g) (d <> h)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

type FourStringIntStringIntAssoc = Four String (Sum Int) String (Product Int) -> Four String (Sum Int) String (Product Int) -> Four String (Sum Int) String (Product Int) -> Bool

-- BoolConj
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj a) <> (BoolConj b) = BoolConj $ a && b

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return $ BoolConj a

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- BoolDisj
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj a) <> (BoolDisj b) = BoolDisj $ a || b

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return $ BoolDisj a

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- Sum type
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  (Fst a) <> (Fst b) = Fst b
  (Fst a) <> (Snd b) = Snd b
  (Snd a) <> (Snd b) = Snd a
  (Snd a) <> (Fst b) = Snd a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return $ Fst a)
              , (1, return $ Snd b)]

type OrStringIntAssoc = Or String (Sum Int) -> Or String (Sum Int) -> Or String (Sum Int) -> Bool

-- Function type
newtype Combine a b = Combine { unCombine :: (a -> b)} deriving (Generic)

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine $ \a -> (f a) <> (g a)

{- TODO: need to figure out how to write the instance for function type
instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- promote (\a -> coarbitrary a arbitrary)
    return $ Combine f
-}
type CombineIntSumIntAssoc = Combine Int (Sum Int) -> Combine Int (Sum Int) -> Combine Int (Sum Int) -> Bool

-- Comp
newtype Comp a = Comp { unComp :: (a -> a) } deriving (Generic)

instance (Semigroup a) => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp $ \a -> f a <> g a

f = \x -> Sum $ getSum x + 1
g = \x -> Sum $ getSum x - 2

compF = Comp f
compG = Comp g

-- Validation
data Validation a b = Failure a | Success b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Validation a b) where
  Success b1 <> Success b2 = Success $ b2
  Failure a1 <> Success b2 = Failure a1
  Success b1 <> Failure a2 = Failure a2
  Failure a1 <> Failure a2 = Failure $ a1

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return $ Failure a)
              , (1, return $ Success b)]

type ValidationStringSumIntAssoc = Validation String (Sum Int) -> Validation String (Sum Int) -> Validation String (Sum Int) -> Bool

-- Validation with a Semigroup that accumulate success
newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  (AccumulateRight (Success b1)) <> (AccumulateRight (Success b2)) = AccumulateRight $ Success $ b1 <> b2
  (AccumulateRight (Failure a)) <> _ = AccumulateRight $ Failure a
  _ <> (AccumulateRight (Failure a)) = AccumulateRight $ Failure a

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return $ AccumulateRight $ Failure a)
              , (1, return $ AccumulateRight $ Success b)]

type AccumulateRightStringSumIntAssoc = AccumulateRight String (Sum Int) -> AccumulateRight String (Sum Int) -> AccumulateRight String (Sum Int) -> Bool

-- Validation with a Semigroup that accumulate both failure and success
newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  (AccumulateBoth (Failure a1)) <> (AccumulateBoth (Failure a2)) = AccumulateBoth $ Failure $ a1 <> a2
  (AccumulateBoth (Success b1)) <> (AccumulateBoth (Success b2)) = AccumulateBoth $ Success $ b1 <> b2
  (AccumulateBoth (Failure a)) <> _ = AccumulateBoth $ Failure a
  _ <> (AccumulateBoth (Failure a)) = AccumulateBoth $ Failure a

type AccumulateBothStringSumIntAssoc = AccumulateBoth String (Sum Int) -> AccumulateBoth String (Sum Int) -> AccumulateBoth String (Sum Int) -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityStringAssoc)
  quickCheck (semigroupAssoc :: TwoStringIntAssoc)
  quickCheck (semigroupAssoc :: ThreeStringIntIntAssoc)
  quickCheck (semigroupAssoc :: FourStringIntStringIntAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrStringIntAssoc)
  -- TODO: quickcheck the funciton type
  -- quickCheck (semigroupAssoc :: CombineIntSumIntAssoc)
  quickCheck (semigroupAssoc :: ValidationStringSumIntAssoc)
  quickCheck (semigroupAssoc :: AccumulateRightStringSumIntAssoc)
  quickCheck (semigroupAssoc :: AccumulateBothStringSumIntAssoc)
