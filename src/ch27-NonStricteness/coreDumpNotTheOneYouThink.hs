-- two ways of examining stricteness of Haskell
-- 1. injecting bottom
-- 2. examing GHC core

discriminatory :: Bool -> Int
discriminatory b =
  case b of
    True -> 1
    False -> 0

-- on stack ghci, run ':set -ddump-simpl'
