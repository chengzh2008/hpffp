module MonoidExercise where

import Test.QuickCheck
import Data.Monoid hiding ((<>))
import Data.Semigroup

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Semigroup m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Semigroup m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

main :: IO ()
main = do
  -- Trivial
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: TrivialIdent)
  quickCheck (monoidRightIdentity :: TrivialIdent)
  -- Identity a
  quickCheck (semigroupAssoc :: IdentityStringAssoc)
  quickCheck (monoidLeftIdentity :: IdentityIdent)
  quickCheck (monoidRightIdentity :: IdentityIdent)
  -- Two a b
  quickCheck (semigroupAssoc :: TwoStringSumIntAssoc)
  quickCheck (monoidLeftIdentity :: TwoStringSumIntIdent)
  quickCheck (monoidRightIdentity :: TwoStringSumIntIdent)
  -- BoolConj
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  -- BoolDisj
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)




-- Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
type TrivialIdent = Trivial -> Bool

-- Identity
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity a1 <> Identity a2 = Identity $ a1 <> a2

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

type IdentityStringAssoc = Identity String -> Identity String -> Identity String -> Bool
type IdentityIdent = Identity String -> Bool

-- product: Two a b
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a1 b1 <> Two a2 b2 = Two (a1 <> a2) (b1 <> b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

type TwoStringSumIntAssoc = Two String (Sum Int) -> Two String (Sum Int) -> Two String (Sum Int) -> Bool
type TwoStringSumIntIdent = Two String (Sum Int) -> Bool

-- BoolConj
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj b1 <> BoolConj b2 = BoolConj $ b1 && b2

instance Arbitrary BoolConj where
  arbitrary = do
    b <- arbitrary
    return $ BoolConj b

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- BoolDisj
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj b1 <> BoolDisj b2 = BoolDisj $ b1 || b2

instance Arbitrary BoolDisj where
  arbitrary = do
    b <- arbitrary
    return $ BoolDisj b

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- Combine a b
newtype Combine a b = Combine {unCombine :: (a -> b)}

instance Semigroup b => Semigroup (Combine a b) where
  cf <> cg = Combine $ \x -> unCombine cf x <> unCombine cg x

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
  mempty = Combine $ \x -> mempty
  mappend = (<>)

comb = Combine $  (\n -> Sum (n + 1)) :: Combine Int (Sum Int)
--- example: unCombine (mappend f mempty) $ 1
--- Sum {getSum = 2}

-- Comp a
newtype Comp a = Comp (a -> a)

instance Semigroup a => Semigroup (Comp a) where
  Comp f <> Comp g = Comp $ \x -> f x <> g x

instance (Semigroup a, Monoid a) => Monoid (Comp a) where
  mempty = Comp $ \x -> mempty
  mappend = (<>)

runComp :: Comp a -> (a -> a)
runComp (Comp f) = f

comp = Comp $ (\x -> reverse x) :: Comp String
