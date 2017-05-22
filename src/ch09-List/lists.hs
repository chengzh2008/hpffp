module Lists where

myTail :: [a] -> [a]
myTail [] = []
myTail (_:xs) = xs

--safeTail with Maybe
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:[]) = Nothing
safeTail (_:xs) = Just xs
