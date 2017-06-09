module MyOwnEither where

lefts' :: [Either a b] -> [a]
lefts' = foldr go []
  where go :: Either a b -> [a] -> [a]
        go (Right _) acc = acc
        go (Left a) acc = a:acc

rights' :: [Either a b] -> [b]
rights' = foldr go []
  where go :: Either a b -> [b] -> [b]
        go (Left _) bcc = bcc
        go (Right b) bcc = b:bcc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr go ([], [])
  where go :: Either a b -> ([a], [b]) -> ([a], [b])
        go (Left a) (acc, bcc) = (a:acc, bcc)
        go (Right b) (acc, bcc) = (acc, b:bcc)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left _) = Nothing
eitherMaybe' f (Right b) = Just $ f b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b


eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
ehtherMaybe'' g (Left _) = Nothing
eitherMaybe'' g (Right b) = Just $ g b
