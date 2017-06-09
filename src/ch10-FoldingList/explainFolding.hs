module ExplainFolding where

sum' :: [Integer] -> Integer
sum'[] = 0
sum' (x:xs) = x + sum' xs

length' :: [a] -> Integer
length' [] = 0
length' (x:xs) = 1 + length' xs

product' :: [Integer] -> Integer
product' [] = 1
product' (x:xs) = x * product' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- abstract the pattern above
myFolding :: (a -> b ->b) -> b -> [a] -> b
myFolding _ b [] = b
myFolding f b (x:xs) = x `f` myFolding f b xs

-- rewrite with myFolding

sum'' :: [Integer] -> Integer
sum'' = myFolding (+) 0

length'' :: [a] -> Integer
length'' = myFolding (\_ n -> n + 1) 0

product'' :: [Integer] -> Integer
product'' = myFolding (*) 1

concat'' :: [[a]] -> [a]
concat'' = myFolding (++) []


--visulize the folding process
{- 
  example: foldr (visual "*") "1" (map show [1..10])
  >> λ:-) mapM_ putStrLn $ scanr (visual "+") "0" (map show [1..10])

  foldr (visual "+") "0" (map show [1..10])
  = "(1+(2+(3+(4+(5+(6+(7+(8+(9+(10+0))))))))))"o

  evaluation process:
  -> 0
  -> (10+0)
  -> (9+(10+0))
  -> (8+(9+(10+0)))
  -> (7+(8+(9+(10+0))))
  -> (6+(7+(8+(9+(10+0)))))
  -> (5+(6+(7+(8+(9+(10+0))))))
  -> (4+(5+(6+(7+(8+(9+(10+0)))))))
  -> (3+(4+(5+(6+(7+(8+(9+(10+0))))))))
  -> (2+(3+(4+(5+(6+(7+(8+(9+(10+0)))))))))
  -> (1+(2+(3+(4+(5+(6+(7+(8+(9+(10+0))))))))))
-}

visual :: String -> String -> String -> String
visual op x y = concat ["(", x, op, y, ")"]

-- Folding exercise
{-
foldr (++) "" ["woot", "WOOT"]
foldr max ' ' "fear is the little death"
foldr (&&) True [False, True]
foldr (||) True [False, True]
foldl (++) "" $ map show [1..5]
foldr const "a" $ map show [1..5]

-}
