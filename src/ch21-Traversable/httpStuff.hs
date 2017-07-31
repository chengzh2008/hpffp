module HttpStuff where

import Data.ByteString.Lazy hiding (map)
import Network.Wreq

a = "http://www.cnn.com"
urls :: [String]
urls = [ a ]

mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls
