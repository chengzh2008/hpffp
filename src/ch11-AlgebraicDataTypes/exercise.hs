module Exercise where
import Data.Char

-- As-patterns

f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
  print a
  return t

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf as bs =
  and $ map (`elem` bs) as

capitalizeWords :: String -> [(String, String)]
capitalizeWords xs =
  zip wordsList $ map (\s -> toUpper (head s) : tail s) wordsList
  where wordsList = words xs

capitalizeWord :: String -> String
capitalizeWord xs@(x: _) = toUpper x : tail xs

capitalizeParagraph :: String -> String
capitalizeParagraph xss = unwords $ go wordsList
  where
    go :: [String] -> [String]
    go [] = []
    go (w:[]) = [w]
    go (x:y:xs)
      | last x == '.' = x: capitalizeWord y : go xs
      | otherwise = x: go (y:xs)
    wordsList = words xss
