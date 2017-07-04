{-# LANGUAGE RankNTypes #-}
-- transform only the structure and leave the value untouched

type Nat f g = forall a. f a -> g a

-- this will work
maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- also work
maybeToList' :: Maybe a -> [a]
maybeToList' Nothing = []
maybeToList' (Just a) = [a]

-- won't work
{-
degenerateMtl :: Nat Maybe []
degenerateMtl Nothing = []
degenerateMtl (Just a) = [a + 1]
-}
