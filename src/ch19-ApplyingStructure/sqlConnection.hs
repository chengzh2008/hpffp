{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, OverloadedStrings, GADTs #-}
module SqlConnection where

import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Data.Text.Encoding (encodeUtf8)

import Web.Scotty
import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql

import Data.Monoid ((<>))

runDb :: SqlPersist (ResourceT IO) a -> IO a
runDb query = do
  let connStr = foldr (\(k,v) t -> t <> (encodeUtf8 $ k <> "=" <> v <> " ")) "" params
  runResourceT . withPostgresqlConn connStr
