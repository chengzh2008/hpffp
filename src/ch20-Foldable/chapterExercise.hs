-- implement Foldable instances for following datatypes

data Constant a b = Constant a deriving (Eq, Show)

instance Foldable (Constant a) where
  foldr _ z _ = z
  foldMap _ _ = mempty


data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
  foldr f z (Two a b) = f b z
  foldMap f (Two a b) = f b

data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
  foldr f z (Three a b c) = f c z
  foldMap f (Three a b c) = f c

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldr f z (Three' a b1 b2) = f b2 (f b1 z)
  foldMap f (Three' a b1 b2) = f b1 `mappend` f b2

data Four' a b = Four' a b b b deriving (Eq, Show)

instance Foldable (Four' a) where
  foldr f z (Four' a b1 b2 b3) = f b3 ( f b2 ( f b1 z))
  foldMap f (Four' a b1 b2 b3) = f b1 `mappend` f b2 `mappend` f b3

-- using foldMap
-- example:   filterF (>3) [2, 4] :: [Int]     -- answer [4]
-- example:   filterF (elem 'a') (3, "abc")    -- answer "abc"
-- example:   filterF (elem 'a') (3, "bc")    -- answer ""

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f ta = foldMap (\ a -> if f a then pure a else mempty) ta
