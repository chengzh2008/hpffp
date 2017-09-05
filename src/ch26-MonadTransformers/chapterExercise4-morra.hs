{-# LANGUAGE OverloadedStrings #-}
module HitCounter where

-- Hit counter

import Control.Monad.Trans.Class
import Control.Monad.Trans.State

data Score =
  Score { computer :: Integer
        , human :: Integer
        }


-- TODO: finish the game
main :: IO ()
main = undefined
