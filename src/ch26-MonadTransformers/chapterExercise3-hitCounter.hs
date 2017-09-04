{-# LANGUAGE OverloadedStrings #-}
module HitCounter where

-- Hit counter

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config =
  Config { counts :: IORef (M.Map Text Integer)
         , prefix :: Text
         }

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer)
bumpBoomp k m = (M.insert k bump m, bump)
  -- 0 here is the default count is the key is not in the map
  where bump = (fromMaybe 0 (M.lookup k m)) + 1

app :: Scotty ()
app = get "/:key" $ do
  unprefixed <- param "key"
  config <- lift ask
  let key' = mappend (prefix config) unprefixed
      ref = counts config
      map' = readIORef ref -- IO (M.Map Text Integer) thus use <$> next line
  (newMap, newInteger) <- liftIO (bumpBoomp key' <$> map')
  liftIO $ print "map updated: "
  liftIO $ print newMap
  liftIO $ writeIORef ref newMap
  html $ mconcat [ "<h1>Success! key was: "
                 , key'
                 , " and Count was: "
                 , TL.pack $ show newInteger
                 , "</h1"]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config { counts = counter
                      , prefix = TL.pack prefixArg}
      -- runR :: (ReaderT Config IO) Response -> IO Response
      runR r = runReaderT r config -- here config is the input for Reader type
  scottyT 3000 runR app
