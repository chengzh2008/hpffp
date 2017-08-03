module ReaderPractice where

import Control.Applicative
import Data.Maybe
import Data.Monoid

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]


xy = zipWith (,) x y
yz = zipWith (,) y z
xz = zipWith (,) x z

xs :: Maybe Integer
xs = lookup 3 $ xy

ys :: Maybe Integer
ys = lookup 6 $ yz

zs :: Maybe Integer
zs = lookup 4 $ xy

z' :: Integer -> Maybe Integer
z' n = lookup n xz

x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)

all' :: [Bool] -> Bool
all' = getAll . foldMap All

s' = summed <$> ((,) <$> xs <*> ys)

seqA :: Integral a => a -> [Bool]
seqA = sequenceA [(>3), (<8), even]

-- TODO: rewrite Shawty to use ReaderT
-- template: https://github.com/bitemyapp/shawty-prime/blob/master/app/Main.hs

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ all' $ sequenceA [(>3), (<8), even] 6
  print $ all' $ sequenceA [(>3), (<8), even] 7
  print $ seqA $ fromMaybe 0 s'
  print $ bolt $ fromMaybe 0 ys
