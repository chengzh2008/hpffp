import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck
import Data.Monoid ((<>))

type EitherTypeTest = Either' String (Int, Int, String)
type TupleTypeTest = Tuple String (Int, Int, String)

main :: IO ()
main = do
  quickBatch (traversable (undefined :: EitherTypeTest))
  quickBatch (traversable (undefined :: TupleTypeTest))

-- Travesable instance for Either' a b datatype
data Either' a b = Left' a | Right' b deriving (Eq, Show)

instance Functor (Either' a) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap _ (Left' a) = Left' a
  fmap f (Right' b) = Right' $ f b

instance Applicative (Either' a) where
  -- pure :: Applicative f => a -> f a
  pure b = Right' b
  -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
  Left' a <*> _ = Left' a
  Right' fb <*> r = fmap fb r

instance Foldable (Either' a) where
  -- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
  foldMap _ (Left' a) = mempty
  foldMap f (Right' b) = f b

  -- foldr :: (Foldable t) => (a -> b -> c) -> b -> t a -> c
  foldr _ mempty (Left' a) = mempty
  foldr f z (Right' b) = f b z

instance Traversable (Either' a) where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse _ (Left' a) = pure (Left' a)
  traverse f (Right' b) = Right' <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Either' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Left' a), (1, return $ Right' b)]

instance (Eq a, Eq b) => EqProp (Either' a b) where
  (=-=) = eq


-- Traversable instance for Tuple a b datatype
data Tuple a b = Tuple a b deriving (Eq, Show)

instance Functor (Tuple a) where
  fmap f (Tuple a b) = Tuple a $ f b

instance (Monoid a) => Applicative (Tuple a) where
  pure b = Tuple mempty b
  Tuple a fb <*> Tuple a1 b = Tuple (a <> a1) (fb b)

instance Foldable (Tuple a) where
  foldMap f (Tuple _ b) = f b
  foldr f z (Tuple _ b) = f b z


instance Traversable (Tuple a) where
  traverse f (Tuple a b) = Tuple a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Tuple a b

instance (Eq a, Eq b) => EqProp (Tuple a b) where
  (=-=) = eq
