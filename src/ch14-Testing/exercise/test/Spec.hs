import Test.Hspec
import WordNumber (digitToWord, digits, wordNumber)
import Test.QuickCheck
import Data.List

main :: IO ()
main = do
  hspec $ do
    describe "digitToWord" $ do
      it "returns zero for 0" $ do
        digitToWord 0 `shouldBe` "Zero"
      it "returns One for 1" $ do
        digitToWord 1 `shouldBe` "One"
    describe "digits" $ do
      it "returns [1] for 1" $ do
        digits 1 `shouldBe` [1]
      it "returns [1, 0, 0] for 100" $ do
        digits 100 `shouldBe` [1, 0, 0]
    describe "wordNumber" $ do
      it "one-sero-zero given 100" $ do
        wordNumber 100 `shouldBe` "One-Zero-Zero"
      it "nine-zero-zero-one for 9001" $ do
        wordNumber 9001 `shouldBe` "Nine-Zero-Zero-One"
    describe "function test" $ do
      it "half x is always x/2" $ do
        property $ prop_half
  quickCheck prop_half
  quickCheck plusAssociative
  quickCheck multipleAssociative
  quickCheck quotRemLaw
  quickCheck divModLaw
  -- not hold
  quickCheck powerNotAssociate
  quickCheck (reverseList :: [Integer] -> Bool)
  -- idempotence
  quickCheck (f1 :: [String] -> Bool)
  quickCheck (f2 :: [Integer] -> Bool)


half :: (Fractional a) => a -> a
half x = x / 2

prop_half :: Double -> Bool
prop_half x = ((*2) . half $ x) == x

--plusAssociative
plusAssociative ::  Integer -> Integer -> Integer -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

multipleAssociative :: Integer -> Integer -> Integer -> Bool
multipleAssociative x y z = x * (y * z) == (x * y) * z

quotRemLaw :: Integer -> Integer -> Bool
quotRemLaw x y
  | y /= 0 = (quot x y) * y + rem x y == x
  | otherwise = True

divModLaw :: Integer -> Integer -> Bool
divModLaw x y
  | y /= 0 = (div x y) * y + mod x y == x
  | otherwise = True

powerNotAssociate :: Integer -> Integer -> Integer -> Bool
powerNotAssociate x y z = x ^ (y ^ z) == (x ^ y) ^ z

-- twice reverse of list is itself
reverseList :: Eq a => [a] -> Bool
reverseList x = (reverse . reverse $ x) == id x

-- $ property
dollarSign :: Eq a => (a -> a) -> a -> Bool
dollarSign f a = (f $ a) == f a


-- idempotence
twice f = f . f
fourTimes = twice . twice

f1 x = (sort x) == twice sort x
f2 x = (sort x) == fourTimes sort x
