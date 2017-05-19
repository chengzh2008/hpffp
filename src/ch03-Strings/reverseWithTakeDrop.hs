module Reverse where

-- revers words in a sentence
rvrs :: String -> String
rvrs "" = ""
rvrs x = if second == "" then first else second ++ " " ++ first
  where
    notSpace = (/=) ' '
    first = takeWhile notSpace x
    left = dropWhile notSpace x
    rest "" = ""
    rest (z:zs) = zs
    second = rvrs (rest left)
