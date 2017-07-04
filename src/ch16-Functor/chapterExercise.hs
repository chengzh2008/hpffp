{-# LANGUAGE FlexibleInstances #-}
-- Quant
data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor (f b)
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a

-- Phantom
data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K a) = K a

-- flip
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K $ f a

-- Some Const
data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

-- functor wrapper
data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance (Functor f) => Functor (LiftItOut f) where
  fmap g (LiftItOut fa) = LiftItOut $ fmap g fa

-- functor wrapper 2
data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap h (DaWrappa fa ga) = DaWrappa (fmap h fa) (fmap h ga)
