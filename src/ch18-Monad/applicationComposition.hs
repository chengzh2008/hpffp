module ApplicationComposition where

-- fmap id = id
-- fmap (f . g) = fmap f . (fmap g)

-- mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
-- mcomp mbc mab a = mab a >>= mbc
--
-- fish :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
-- fish mab mbc a = mab a >>= mbc
