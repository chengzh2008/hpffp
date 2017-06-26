import Data.Monoid
import Test.QuickCheck
import Control.Monad

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = a == (mempty <> a)


-- test that Bull is not a monoid

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools)
                        , (1, return Twoo)]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool
type StringMappend = String -> String -> String -> Bool

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend  Nada (Only a) = Only a
  mappend  (Only a) Nada = Only a
  mappend (Only a) (Only b) = Only $ a <> b
  mappend _ _ = Nada

instance Arbitrary (Optional a) where
  arbitrary = frequency [ (1, return Nada)
                        , (1, return Only arbitrary)]

newtype First' a = First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Arbitrary (First' a) where
  arbitrary = return First' arbitrary

instance Monoid (First' a) where
  mempty = First' $ empty
  mappend mempty a = a
  mappend a mempty = a
  mappend (First' a) (First' b) = First' a

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

main :: IO ()
main = do
  -- test String Monoid
  quickCheck (monoidAssoc :: StringMappend)
  quickCheck (monoidLeftIdentity :: String -> Bool)
  quickCheck (monoidRightIdentity :: String -> Bool)
  -- test Bull is not a monoid
  quickCheck (monoidAssoc :: BullMappend)
  quickCheck (monoidLeftIdentity :: Bull -> Bool)
  quickCheck (monoidRightIdentity :: Bull -> Bool)
