import Test.Hspec
import WordNumber (digitToWord, digits, wordNumber)
import Test.QuickCheck

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


half :: (Fractional a) => a -> a
half x = x / 2

prop_half :: Double -> Bool
prop_half x = ((*2) . half $ x) == x

--plusAssociative
plusAssociative ::  Double -> Double -> Double -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z
