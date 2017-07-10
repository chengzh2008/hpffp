module ZipListApplicative where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Eq a => EqProp (ZipList a) where
  (=-=) = eq


main :: IO ()
main = do
  quickBatch $ monoid trigger
    where trigger = undefined :: ZipList (Sum Int)
  -- test applicative for ZipList'
  -- TODO: why parse error
  quickBatch $ applicative trigger
    where trigger = undefined :: ZipList' (String, Maybe String, Sum Int)

-- List Applicative implementation
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a ls) = Cons (f a) (fmap f ls)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append`ys

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fs <*> ys = fmap f ys `append` (fs <*> ys)

instance Eq a => EqProp (List a) where
  (=-=) = eq

-- implement the ZipList applciative yourself
take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 la = Nil
take' n (Cons a as) = Cons a $ take' (n- 1) as

-- new type for zipList
newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    return $ Cons a Nil

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    a <- arbitrary
    return $ ZipList' a

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys =  xs' =-= ys'
    where xs' = let (ZipList' l) = xs in take' 3000 l
          ys' = let (ZipList' l) = ys in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

zipListWith :: (a -> b -> c) -> List a -> List b -> List c
zipListWith _  Nil _ = Nil
zipListWith _ _ Nil = Nil
zipListWith f (Cons a as) (Cons b bs) = Cons (f a b) (zipListWith f as bs)

-- manual test to confirm it is correct
instance Applicative ZipList' where
  pure = pure
  (ZipList' Nil) <*> _ = ZipList' Nil
  _ <*> (ZipList' Nil) = ZipList' Nil
  (ZipList' as) <*> (ZipList' bs) = ZipList' $ zipListWith ($) as bs
