{-# LANGUAGE DeriveGeneric #-}
module CoArbitraryTypeclass where

import GHC.Generics
import Test.QuickCheck

data Bool' = True' | False' deriving (Generic)

instance CoArbitrary Bool'

trueGen :: Gen Int
trueGen = coarbitrary True' arbitrary
