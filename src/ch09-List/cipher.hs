module Cipher where

import Data.Char

cipher :: Int -> Char -> Char
cipher _ ' ' = ' '
cipher _ '!' = '!'
cipher n c = chr $ mod (ord c + n - base) 26 + base
  where base = ord 'a'

decipher :: Int -> Char -> Char
decipher n = cipher (26 - n)

decipher' :: String -> [String]
decipher' encrypted = map f [1..26]
  where
    f :: Int -> String
    f = \n -> map (cipher n) encrypted

numToChar :: Int -> Char
numToChar = chr
