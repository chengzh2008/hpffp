import Data.Monoid

-- Identity
newtype Identity a = Identity a deriving (Eq, Show, Ord)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure a = Identity a
  Identity f <*> Identity a = Identity $ f a

-- Constant a b
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant e) = Constant e

instance Monoid a => Applicative (Constant a) where
  pure x = Constant $ mempty
  Constant a1 <*> Constant a2 = Constant $ a1 <> a2
