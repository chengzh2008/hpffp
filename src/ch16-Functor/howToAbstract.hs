-- more specific
incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just $ n + 1
incIfJust Nothing = Nothing

showIfJust :: Show a => Maybe a -> Maybe String
showIfJust (Just s) = Just $ show s
showIfJust Nothing = Nothing

-- using fmap

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe m = fmap (+1) m

showMaybe :: Show a => Maybe a -> Maybe String
showMaybe m = fmap show m


-- more general
incMaybe' :: Num a => Maybe a -> Maybe a
incMaybe' = fmap (+1)

showMaybe' :: Show a => Maybe a -> Maybe String
showMaybe' = fmap show


-- polymorphic
liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show


-- exercise
data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers $ f a

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second $ f b

-- create FlipSum that applies fucntion to First a but not Second b
newtype FlipSum a b = FlipSum (Sum b a) deriving (Eq, Show)

instance Functor (FlipSum a) where
  fmap f (FlipSum (First b)) = FlipSum $ First $ f b
  fmap f (FlipSum (Second a)) = FlipSum $ Second a
