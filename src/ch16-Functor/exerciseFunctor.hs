import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) =>
                      f a
                   -> Fun a b
                   -> Fun b c
                   -> Bool
functorCompose x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)


main :: IO ()
main = do
  -- Identity functor
  quickCheck (functorIdentity :: IdentityIdent)
  quickCheck (functorCompose :: IdentityCompose)
  -- Pair functor
  quickCheck (functorIdentity :: PairIdent)
  quickCheck (functorCompose :: PairCompose)
  -- Two functor
  quickCheck (functorIdentity :: TwoIdent)
  quickCheck (functorCompose :: TwoCompose)
  -- Three functor
  quickCheck (functorIdentity :: ThreeIdent)
  quickCheck (functorCompose :: ThreeCompose)
  -- Four functor
  quickCheck (functorIdentity :: FourIdent)
  quickCheck (functorCompose :: FourCompose)
  -- Four' functor
  quickCheck (functorIdentity :: FourPriIdent)
  quickCheck (functorCompose :: FourPriCompose)


-- Identity functor
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor (Identity) where
  fmap f (Identity a) = Identity $ f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentityIdent = Identity Int -> Bool
type IdentityCompose = Identity Int -> (Fun Int String) -> (Fun String Char) -> Bool

-- Pair a
data Pair a = Pair a a deriving (Eq, Show)

instance Functor (Pair) where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    return $ Pair a1 a2

type PairIdent = Pair String -> Bool
type PairCompose = Pair String -> (Fun String Char) -> (Fun Char Int) -> Bool

-- Two a b
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoIdent = Two String Int -> Bool
type TwoCompose = Two String Int -> (Fun Int String) -> (Fun String Char) -> Bool

-- Three a b c
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeIdent = Three (Maybe Int) String Int -> Bool
type ThreeCompose = Three (Maybe Int) String Int
                      -> (Fun Int String)
                      -> (Fun String Char)
                      -> Bool

-- Four a b c d
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

type FourIdent = Four (Maybe Int) String [Char] Int -> Bool
type FourCompose = Four (Maybe Int) String [Char] Int
                      -> (Fun Int String)
                      -> (Fun String Char)
                      -> Bool


-- Four' a b
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    a3 <- arbitrary
    b <- arbitrary
    return $ Four' a1 a2 a3 b

type FourPriIdent = Four' (Maybe Int) String -> Bool
type FourPriCompose = Four' (Maybe Int) String
                      -> (Fun String Int)
                      -> (Fun Int Char)
                      -> Bool
