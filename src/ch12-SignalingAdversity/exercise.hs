module Exercise where

--String processing

notThe :: String -> Maybe String
notThe s
  | s == "the" = Nothing
  | otherwise = Just s

replaceThe :: String -> String
replaceThe = unwords . (map change) . (map notThe) . words
  where change :: Maybe String -> String
        change Nothing = "a"
        change (Just s) = s


-- count the before vowel
vowels = ['a', 'e', 'i', 'o', 'u']
--vowels = "aeiou"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = count . words
  where count :: [String] -> Integer
        count [] = 0
        count (x:[]) = 0
        count (x:y:xs)
          | x == "the" && (elem (head y) vowels) = 1 + count xs
          | otherwise = count $ y:xs

 -- validate words

newtype Word' = Word' String deriving (Eq, Show)

countVowels :: String -> Int
countVowels [] = 0
countVowels (x:xs)
  | elem x vowels = 1 + countVowels xs
  | otherwise = countVowels xs

mkWord :: String -> Maybe Word'
mkWord s
  | vowelsCount > length s - vowelsCount = Nothing
  | otherwise = Just $ Word' s
  where vowelsCount = countVowels s


-- It's only natural
data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0 = Nothing
  | otherwise = Just $ go n
  where go 0 = Zero
        go m = Succ $ go $ m -1
