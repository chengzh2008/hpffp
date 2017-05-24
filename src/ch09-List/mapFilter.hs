module MapFilter where

multi3 :: [Int] -> [Int]
multi3 = filter (\x -> rem x 3 == 0)

lenMulti3 = length . multi3

normalize :: String -> [String]
normalize = filter (\x -> not $ elem x ["the", "a", "an"]) . words
