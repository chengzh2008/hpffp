module Main where

import qualified Database.Redis as R
import Web.Scotty
import Lib

main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo
  scotty 3000 (app rConn)
