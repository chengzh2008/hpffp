module TakeDropWhile where

isSpace :: Char -> Bool
isSpace = \x -> x == ' '

-- using only takeWhile and dropWhile to split string by space
myWords :: String -> [String]
myWords "" = []
myWords s = first : myWords (dropWhile isSpace rest)
  where first = takeWhile (not . isSpace) s
        rest = dropWhile (not . isSpace) s

-- seems there is a bug. need further test.
splitBy :: String -> Char -> [String]
splitBy [] seperator =  []
splitBy s seperator
  | first == [] = myWords (dropWhile isSeperator rest)
  | otherwise = first : myWords (dropWhile isSeperator rest)
  where
        isSeperator = \x -> x == seperator
        first = takeWhile (not . isSeperator) s
        rest = dropWhile (not . isSeperator) s

-- (1, 2) WHNF & NF
-- (1, 1 + 1) WHNF
-- \x -> x * 10 WHNF & NF
-- "Papu" ++ "chon" not a WHNF nor NF becuase the outmost component is a funciton not a constructor
-- (1, "abc" ++ "cde") WHNF

-- exercise:

-- [1, 2, 3, 4, 5] WHNF & NF
-- 1 : 2: 3: 4: _ WHNF
-- enumFromTo 1 10 not WHNF not NF
-- length [1, 2] not WHNF not NF
-- SUM (enumFromTo 1 10) not WHNF not NF
-- ['a'..'m'] ++ ['n'..'z'] not WHNF not NF
--  (_, 'b') WHNF
