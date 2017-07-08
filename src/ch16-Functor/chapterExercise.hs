{-# LANGUAGE FlexibleInstances #-}

import Text.Show.Functions

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

-- functor IgnoreOne
data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap h (IgnoringSomething fa gb) = IgnoringSomething fa $ fmap h gb

-- functor Notorious goat
data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga $ fmap f gt

-- functor List
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a ls) = Cons (f a) $ fmap f ls

-- functor Tree
data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats ga1 ga2 ga3) = MoreGoats (fmap f ga1) (fmap f ga2) (fmap f ga3)

-- TODO: functor of a Mixture
data TalkToMe a = Halt | Print String a | Read {unRead::String -> a} deriving (Show)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s $ f a
  fmap f (Read g) = Read (f . g)
