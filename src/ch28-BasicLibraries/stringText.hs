module Main where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified System.IO as SIO

path :: String
path = "/usr/share/dict/words"

dictWords :: IO String
dictWords = SIO.readFile path

dictWordsT :: IO T.Text
dictWordsT = TIO.readFile path

dictWordsTL :: IO TL.Text
dictWordsTL = TLIO.readFile path

main :: IO ()
main = do
  replicateM_ 10 (dictWords >>= print)
  replicateM_ 10 (dictWordsT >>= TIO.putStrLn)
  replicateM_ 10 (dictWordsTL >>= TLIO.putStrLn)

-- run the following command to profile
{-
stack ghc -- -prof -fprof-auto -rtsopts -O2 stringText.hs
./stringText +RTS -hc -p
hp2ps stringText.hp
-}
