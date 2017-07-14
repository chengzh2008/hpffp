{-# LANGUAGE InstanceSigs #-}
module MoreApplicative where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Control.Applicative (liftA3)

-- Pair a a
data Pair a = Pair a a deriving (Eq, Show)

instance Functor (Pair) where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance Applicative (Pair) where
  pure a = Pair a a
  Pair fa fb <*> Pair a b = Pair (fa a) (fb b)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    return $ Pair a1 a2

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq

-- Two a b
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Monoid a) => Applicative (Two a) where
  pure b = Two mempty b
  Two a1 fb <*> Two a2 b = Two (a1 <> a2) (fb b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

-- Three a b c
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three mempty mempty c
  Three a1 b1 fc <*> Three a2 b2 c = Three (a1 <> a2) (b1 <> b2) (fc c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- Three' a b
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance (Monoid a) => Applicative (Three' a) where
  pure b = Three' mempty b b
  Three' a1 fb1 fb2 <*> Three' a2 b1 b2 = Three' (a1 <> a2) (fb1 b1) (fb2 b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    return $ Three' a b1 b2

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

-- Four a b c d
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure d = Four mempty mempty mempty d
  Four a1 b1 c1 fd <*> Four a2 b2 c2 d = Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (fd d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

-- Four' a b
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance (Monoid a) => Applicative (Four' a) where
  pure b = Four' mempty mempty mempty b
  Four' a1 a2 a3 fb <*> Four' a4 a5 a6 b = Four' (a1 <> a4) (a2 <> a5) (a3 <> a6) (fb b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    a3 <- arbitrary
    b <- arbitrary
    return $ Four' a1 a2 a3 b

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq



main :: IO ()
main = do
  quickBatch $ applicative (undefined :: Pair (String, Maybe Int, String))
  quickBatch $ applicative (undefined :: Two String (String, Maybe Int, Sum Int))
  quickBatch $ applicative (undefined :: Three String (Maybe String) (String, Maybe Int, Int))
  quickBatch $ applicative (undefined :: Three' String (String, Maybe Int, Int))
  quickBatch $ applicative (undefined :: Four String (Sum Int) (Maybe String) (String, Maybe Int, Int))
  quickBatch $ applicative (undefined :: Four' String (String, Maybe Int, Int))


stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
