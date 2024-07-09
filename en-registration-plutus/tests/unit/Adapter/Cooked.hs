{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Adapter.Cooked (shouldViolateAProperty) where

import Cooked
import Cooked.MockChain.Staged
import Data.Default (Default (def))
import PlutusTx.Builtins
import Test.Tasty.QuickCheck (Property)

shouldViolateAProperty
  :: (Show a)
  => BuiltinString
  -> InitialDistribution
  -> StagedMockChain a
  -> Property
shouldViolateAProperty e =
  testFailsFrom @Property
    def
    ( isCekEvaluationFailureWithMsg
        def
        (\a -> show a == show e)
    )

instance Show InitialDistribution where
  show (InitialDistribution xs) = "InitialDistribution : " ++ show xs
