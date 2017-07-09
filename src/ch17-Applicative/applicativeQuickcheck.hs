module BadMonoid where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools)
                        , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend a b
    | a /= mempty && b == mempty = a
    | a == mempty && b /= mempty = b
    | otherwise = a

instance EqProp Bull where
  (=-=) = eq

main :: IO ()
main = do
  -- test monoid of Twoo
  quickBatch (monoid Twoo)
  -- test applicative of list
  quickBatch $ applicative [("b", "w", 1 :: Int)]
  -- or just specify the type
  quickBatch $ applicative (undefined :: [(String, Maybe String, Int)])
  -- or
  quickBatch $ applicative trigger
    where trigger = undefined :: [(String, Maybe String, Int)]
