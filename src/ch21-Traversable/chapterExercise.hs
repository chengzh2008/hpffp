import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck

main :: IO ()
main = do
  quickBatch (traversable (undefined :: IdentityTypeTest))
  quickBatch (traversable (undefined :: ConstantTypeTest))
  quickBatch (traversable (undefined :: OptionalTypeTest))
  quickBatch (traversable (undefined :: ListTypeTest))
  quickBatch (traversable (undefined :: ThreeTypeTest))
  quickBatch (traversable (undefined :: ThreePrimeTypeTest))
  -- TODO: Aarbitrary instance for function type
  -- quickBatch (traversable (undefined :: STypeTest))
  quickBatch (traversable (undefined :: TreeTypeTest))

-- Traversable instance for Identity' a
type IdentityTypeTest = Identity' (Int, Int, String)
data Identity' a = Identity' a deriving (Eq, Show)

instance Traversable Identity' where
  traverse f (Identity' a) = Identity' <$> f a

instance Foldable Identity' where
  foldMap f (Identity' a) = f a

instance Functor Identity' where
  fmap f (Identity' a) = Identity' $ f a

instance Arbitrary a => Arbitrary (Identity' a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity' a

instance Eq a => EqProp (Identity' a) where
  (=-=) = eq


-- Traversable instance for Constant' a b
type ConstantTypeTest = Constant' String (Int, Int, String)
data Constant' a b = Constant' { getConstant :: a } deriving (Eq, Show)

instance Traversable (Constant' a) where
  -- point-free style
  traverse _ = pure . Constant' . getConstant

instance Foldable (Constant' a) where
  foldMap _ = mempty

instance Functor (Constant' a) where
  fmap _ = Constant' . getConstant

instance Arbitrary a => Arbitrary (Constant' a b) where
  arbitrary = do
    a <- arbitrary
    return $ Constant' a
instance Eq a => EqProp (Constant' a b) where
  (=-=) = eq

-- Traversable instance for Optional a
type OptionalTypeTest = Optional (Int, Int, String)
data Optional a = Nada | Yep a deriving (Eq, Show)

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    frequency [ (1, return Nada)
              , (1, return $ Yep a)]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

-- Traversable instance for List a
type ListTypeTest = List (Int, Int, String)
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons a as) = Cons <$> f a <*> traverse f as

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons a as) = f a `mappend` foldMap f as

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) $ fmap f as

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    as <- arbitrary
    frequency [ (1, return Nil)
              , (1, return $ Cons a as)]

instance Eq a => EqProp (List a) where
  (=-=) = eq


-- Traversable instance for Three a b c
type ThreeTypeTest = Three Int String (Int, Int, String)
data Three a b c = Three a b c deriving (Eq, Show)

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq


-- Traversable instance for Three' a b
type ThreePrimeTypeTest = Three' Int (Int, Int, String)
data Three' a b = Three' a b b deriving (Eq, Show)

instance Traversable (Three' a) where
  traverse f (Three' a b1 b2) = Three' a <$> f b1 <*> f b2

instance Foldable (Three' a) where
  foldMap f (Three' a b1 b2) = f b1 `mappend` f b2

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    return $ Three' a b1 b2

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq


-- Traversable instance for S n a
{-
type STypeTest = S ((->) String) (String, String, String)
data S n a = S (n a) a deriving (Eq, Show)

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = foldMap f na `mappend` f a

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (fmap f na) (f a)

-- TODO: need to figure out how to write Arbitrary instance of function
instance (Arbitrary n, Arbitrary a) => Arbitrary (S na a) where
  arbitrary = do
    a <- arbitrary
    n <- arbitrary
    return $ S (n a) a

instance (Eq a, Eq (n a)) => EqProp (S n a) where
  (=-=) = eq
-}

-- Traversable instance for Tree
type TreeTypeTest = Tree (Int, Int, String)
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node t1 a t2) = Node <$> traverse f t1 <*> f a <*> traverse f t2

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node t1 a t2) = foldMap f t1 `mappend` f a `mappend` foldMap f t2

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node t1 a t2) = Node (fmap f t1) (f a) (fmap f t2)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    t1 <- arbitrary
    t2 <- arbitrary
    frequency [ (1, return Empty)
              , (1, return $ Leaf a)
              , (1, return $ Node t1 a t2)]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq
