{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta
import Text.RawString.QQ

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator can not be zero"
    _ -> return (numerator % denominator)

parseIntEof :: Parser Integer
parseIntEof = integer >>= (\n -> eof >> return n)

data DecimalOrRational = Le Double | Ri Rational deriving (Eq, Show)

deciOrRati = [r|
1.3
1/3
33/0
333
23.3
|]

-- TODO: not done yet
decimalOrRational :: Parser DecimalOrRational
decimalOrRational = do
  skipMany (oneOf "\n")
  v <- (Le <$> double) <|> (Ri <$> virtuousFraction)
  skipMany (oneOf "\n")
  return v



main :: IO ()
main = do
  print $ parseString parseFraction mempty shouldWork
  print $ parseString parseFraction mempty shouldAlsoWork

  print $ parseString parseFraction mempty alsoBad
  -- print $ parseString parseFraction mempty  badFraction
  print $ parseString virtuousFraction mempty  badFraction
  print $ parseString (some decimalOrRational) mempty deciOrRati
