module ChapterExercise where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad (forM, liftM2)

-- monad for Nope a
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return _ = NopeDotJpg
  NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

-- monad for PhhhbbtttEither
data PhhhbbtttEither a b = Left' a | Right' b deriving (Eq, Show)

instance Functor (PhhhbbtttEither a) where
  fmap _ (Left' a) = Left' a
  fmap f (Right' b) = Right' $ f b

instance (Monoid a) => Applicative (PhhhbbtttEither a) where
  pure b = Right' b
  (Right' fb) <*>  (Right' b) = Right' $ fb b
  (Left' a1) <*> (Left' a2) = Left' (a1 `mappend` a2)
  (Left' a) <*> _ = Left' a
  _ <*> (Left' a) = Left' a

instance (Monoid a) => Monad (PhhhbbtttEither a) where
  return b = Right' b
  Left' a >>= _ = Left' a
  Right' b >>= f = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return $ Left' a)
              , (1, return $ Right' b)]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither a b) where
  (=-=) = eq

-- monad for Identity' a
data Identity' a = Identity' a deriving (Eq, Show)

instance Functor Identity' where
  fmap f (Identity' a) = Identity' $ f a

instance Applicative Identity' where
  pure a = Identity' a
  Identity' fa <*> Identity' a = Identity' $ fa a

instance Monad Identity' where
  return = pure
  (Identity' a) >>= f = f a

instance (Arbitrary a) => Arbitrary (Identity' a) where
  arbitrary = Identity' <$> arbitrary

instance (Eq a) => EqProp (Identity' a) where
  (=-=) = eq

-- monad for List' a
data List' a = Nil | Cons a (List' a) deriving (Eq, Show)

instance Monoid (List' a) where
  mempty = Nil
  (Cons a as) `mappend` bs = Cons a (as `mappend` bs)
  Nil `mappend` bs = bs

instance Functor (List') where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative (List') where
  pure a = Cons a Nil
  (Cons fa fas) <*> as = fmap fa as `mappend` (fas <*> as)

instance Monad (List') where
  return = pure
  Nil >>= _ = Nil
  (Cons a as) >>= f = case (f a) of
                        Nil -> as >>= f
                        Cons b bs -> Cons b bs `mappend` (as >>= f)

instance (Arbitrary a) => Arbitrary (List' a) where
  arbitrary = do
    a <- arbitrary
    frequency [ (1, return Nil)
              , (1, return $ Cons a Nil)]

instance (Eq a) => EqProp (List' a) where
  (=-=) = eq

-- implement the following function
-- with do notation
j :: Monad m => m (m a) -> m a
j mma = do
  ma <- mma
  a <- ma
  return a
-- without do notation
j' :: Monad m => m (m a) -> m a
j' mma = mma >>= (\ma -> ma >>= return)
-- point-free style
j'' :: Monad m => m (m a) -> m a
j'' = flip (>>=) (\ma -> ma >>= return)
-- section
j''' :: Monad m => m (m a) -> m a
j''' = (>>= (\ma -> ma >>= return))

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = ma >>= return . f

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = ma >>= \a -> mf >>= return . ($ a)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh as famb = sequenceA $ fmap famb as

meh' :: Monad m => [a] -> (a -> m b) -> m [b]
meh' = forM

flipType :: (Monad m) => [m a] -> m [a]
flipType = sequenceA
-- TODO: implement it using meh
flipType' :: (Monad m) => [m a] -> m [a]
flipType' ma = undefined


main :: IO ()
main = do
  --Nope test
  quickBatch $ monad (undefined :: Nope (String, Maybe String, [Int]))
  -- PhhhbbtttEither test
  quickBatch $ monad (undefined :: PhhhbbtttEither String (String, Maybe String, [Int]))
  -- Idnentity' a test
  quickBatch $ monad (undefined :: Identity' (String, Maybe String, [Int]))
  -- List' a test
  quickBatch $ monad (undefined :: List' (String, Maybe String, [Int]))
