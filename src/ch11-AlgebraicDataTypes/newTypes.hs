{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module NewTypes where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Show, TooMany)

{- not needed anymore as pragma GeneralizedNewtypeDeriving is used above
instance TooMany Goats where
  tooMany (Goats n) = n > 43
-}

instance TooMany (Int, String) where
  tooMany (n, _) = n > 52

instance TooMany (Int, Int) where
  tooMany (n, n1) = n + n1 > 33

-- not sure what's going on here: it complains that
-- Expected a type, but Num a has kind 'Constraint'
instance TooMany (Num a, TooMany a => (a, a)) where
  tooMany (n, n1) =  n + n1 > 22
