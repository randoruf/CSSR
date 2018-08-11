module Numeric.CSSR.Statistics.Entropy.Rate where

import CSSR.Prelude
import Data.CSSR.State
import Data.CSSR.Alphabet
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS


entropyRate :: Alphabet -> AllStates -> Double
entropyRate a as =
  (-1) * sum (HS.map go as)
  where
    -- FIXME: why is this described as both prob and freq?
    toProb :: State -> Double
    toProb s = HM.lookupDefault 0 s (distribution a as)

    toSDist :: State -> HashMap Event Double
    toSDist = distributionsLookup a as

    go :: State -> Double
    go s = foldl' (stateEntRate (toProb s)) 0 (HM.toList (toSDist s))

    stateEntRate :: Double -> Double -> (Event, Double) -> Double
    stateEntRate freq sEntRate (_, p)
      | p  > 0    = sEntRate + freq * p * logBase 2 p
      | otherwise = sEntRate

