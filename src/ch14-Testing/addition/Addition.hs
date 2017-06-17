module Addition where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = do
  hspec $
    describe "Addition" $ do
      it "15 devided by 3 is 5" $ do
        devideBy 15 3 `shouldBe` (5, 0)
      it "22 devided by 5 is 4 remainder 2" $ do
        devideBy 22 5 `shouldBe` (4, 2)
      it "x + 1 is always greater than x" $ do
        -- run quickcheck through hspec
        property $ \x -> x + 1 > (x :: Integer)
  -- run property check without hspec
  quickCheck prop_additionGreater

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

devideBy :: Integer -> Integer -> (Integer, Integer)
devideBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1..1000]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTupple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTupple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

data Person = Person String Int deriving Show

genPerson :: Gen Person
genPerson = do
  name <- arbitrary :: Gen String
  age <- arbitrary :: Gen Int
  return $ Person name age

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return $ Just a)]
