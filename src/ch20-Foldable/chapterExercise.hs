-- implement Foldable instances for following datatypes

data Constant a b = Constant a deriving (Eq, Show)

instance Foldable (Constant a) where
  foldr _ z _ = z
  foldMap _ _ = mempty


data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
  foldr f z (Two a b) = f b z
  foldMap f (Two a b) = f b
