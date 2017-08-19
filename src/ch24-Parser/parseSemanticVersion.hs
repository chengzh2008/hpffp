{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module ParseSemanticVersion where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Text (Text)
import Test.Hspec
import Text.RawString.QQ
import Text.Trifecta

version1 :: ByteString
version1 = "2.1.1"

version2 :: ByteString
version2 = "1.0.0-x.7.z.92"

version3 :: ByteString
version3 = "1.0.0-alpha+001"

version4 :: ByteString
version4 = "1.0.0+20130313144700"

version5 :: ByteString
version5 = "1.0.0-beta+exp.sha.5114f85"

data NumberOrString = NOSS String | NOSI Integer deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [String]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Eq, Show)

parseNos :: Parser NumberOrString
parseNos = do
  skipMany (oneOf ".")
  v <- (NOSI <$> integer) <|> (NOSS <$> some letter)
  skipMany (oneOf ".")
  return v
parseMeta :: Parser String
parseMeta = do
  skipMany (oneOf ".")
  v <- some letter <|> some digit
  skipMany (oneOf ".")
  return v
parseMetas :: Parser [String]
parseMetas = do
  skipMany (oneOf "+")
  meta <- many parseMeta
  return meta

parseRelease :: Parser [NumberOrString]
parseRelease = do
  skipMany (oneOf "-")
  release <- many parseNos
  return release

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  skipMany (oneOf ".")
  minor <- integer
  skipMany (oneOf ".")
  patch <- integer
  release <- parseRelease
  meta <- parseMetas
  return $ SemVer major minor patch release meta

{-
parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)
-}
maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
  describe "SemVer Parser" $ do
    it "can parse a version with major minor and patch" $ do
      let m = parseByteString parseSemVer mempty version1
          r' = maybeSuccess m
      print m
      r' `shouldBe` (Just $ SemVer 2 1 1 [] [])

    it "can parse a complete version" $ do
      let m = parseByteString parseSemVer mempty version2
          r' = maybeSuccess m
      print m
      r' `shouldBe` (Just $ SemVer 1 0 0 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] [])

    -- TODO: why fail
    it "can parse a version with meta" $ do
      let m = parseByteString parseSemVer mempty version3
          r' = maybeSuccess m
      print m
      r' `shouldBe` (Just $ SemVer 1 0 0 [NOSS "alpha"] ["001"])
