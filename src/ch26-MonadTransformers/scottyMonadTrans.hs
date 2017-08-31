{-# LANGUAGE OverloadedStrings #-}

module Scotty where

import Web.Scotty.Internal.Types (ActionT(..))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy hiding (get)

import Web.Scotty
import Data.Monoid (mconcat)

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    --lift $ putStrLn "hello"
    -- (ActionT . lift . lift . lift) $ putStrLn "hello"
    (ActionT
     . (ExceptT . fmap Right) -- liftM is the same as fmap for monad
     . (ReaderT . const) -- or \m -> ReaderT $ const m
     . \m -> StateT (\s -> do
                        a <- m
                        return (a, s))
      ) $ putStrLn "hello"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]


-- v :: Monad m => m a
-- liftM Just :: Monad m => m a -> m (Maybe a)
-- liftM Just v :: m (Maybe a)
-- MaybeT (liftM Just v) :: MaybeT m a
