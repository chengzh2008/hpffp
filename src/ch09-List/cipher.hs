module Cipher where

import Data.Char

cipher :: Int -> Char -> Char
cipher n c = chr $ mod (ord c + n - base) 26 + base
  where base = ord 'a'

decipher :: Int -> Char -> Char
decipher n = cipher (26 - n)
