import Data.Foldable (foldMap, foldr)
import Data.Monoid
import Data.Maybe
newtype Min a = Min { getMin :: Maybe a } deriving (Eq, Ord, Show)
newtype Max a = Max { getMax :: Maybe a } deriving (Eq, Ord, Show)

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing
  Min (Nothing) `mappend` y = y
  x `mappend` Min (Nothing) = x
  Min (Just x) `mappend` Min (Just y) = Min $ Just (min x y)

instance Ord a => Monoid (Max a) where
  mempty = Max Nothing
  Max (Nothing) `mappend` y = y
  x `mappend` Max (Nothing) = x
  Max (Just x) `mappend` Max (Just y) = Max $ Just (max x y)

sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0

sum'' :: (Foldable t, Num a) => t a -> a
sum'' =  getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = foldr (*) 1

product'' :: (Foldable t, Num a) => t a -> a
product'' = getProduct . foldMap Product

elem' :: (Functor t, Foldable t, Eq a) => a -> t a -> Bool
elem' a = getAny . foldMap Any . fmap (== a)

elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' = any . (==)

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr maybeMin Nothing
  where maybeMin x Nothing = Just x
        maybeMin x (Just y) = Just $ min x y

-- implement with foldMap needs a helper monoid (Min a)
minimum'' :: (Monoid a, Foldable t, Ord a) => t a -> Maybe a
minimum'' = getMin . foldMap (Min . Just)

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr maybeMin Nothing
  where maybeMin x Nothing = Just x
        maybeMin x (Just y) = Just $ max x y

-- implement with foldMap needs a helper monoid (Min a)
maximum'' :: (Monoid a, Foldable t, Ord a) => t a -> Maybe a
maximum'' = getMax . foldMap (Max . Just)

-- using foldr
null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

-- using foldMap
null'' :: (Foldable t) => t a -> Bool
null'' = getAll . foldMap (All . (\_ -> False))

-- using foldr
length' :: (Foldable t) => t a -> Int
length' = foldr (\_ b -> b + 1) 0

-- using foldMap
length'' :: (Foldable t) => t a -> Int
length'' = getSum . foldMap (Sum . (\_ -> 1))

-- using foldr
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (\a b -> a : b) []

-- using foldMap
toList'' :: (Foldable t) => t a -> [a]
toList'' = foldMap (\a -> [a])

-- using foldMap
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-- using foldr
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\a b -> f a <> b) mempty
