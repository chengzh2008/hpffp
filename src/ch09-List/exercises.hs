module Exercise where

import Data.Char

getUpper :: String -> String
getUpper = filter isUpper

capitalize :: String -> String
capitalize "" = ""
capitalize (c:cs) = (toUpper c) : cs

capitalize' :: String -> String
capitalize' "" = ""
capitalize' (c:cs) = toUpper c : capitalize' cs


capFirst :: String -> Char
capFirst = toUpper . head
