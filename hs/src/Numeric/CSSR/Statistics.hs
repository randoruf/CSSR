{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.CSSR.Statistics
  ( module X
  , variation
  , cMu
  ) where

import Numeric.CSSR.Statistics.Entropy.Rate as X
import Numeric.CSSR.Statistics.Entropy.Relative as X
import Numeric.CSSR.Statistics.Entropy.Relative.Rate as X
import Numeric.CSSR.Statistics.F1 as X
import Data.CSSR.State (State, terminal)

import Protolude hiding (State)
import Data.Vector (Vector)
import Data.HashMap.Strict (HashMap)
import Lens.Micro.Platform (view, (^.), _Right)
import qualified Data.Tree.Looping as L
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM (toList)


variation :: HashMap State Double -> Double -> Double
variation dist adjustedDataSize = foldl' go 0 (HM.toList dist)
 where
  go :: Double -> (State, Double) -> Double
  go total (h, inferredProb) = total + abs (historyProb - inferredProb)
   where
    historyProb :: Double
    historyProb = V.sum $ V.map ((/ adjustedDataSize).fromIntegral) (viewFrequency h)

    viewFrequency :: State -> Vector Integer
    viewFrequency s = terminal s ^. (L.bodyL . _Right . L.frequencyL)


-- I think this is the Grassberger-Crutchfield-Young "statistical complexity."
-- I'm taking a shot in the dark at what the name might be.
cMu :: forall f . (Num f, Floating f) => Vector f -> f
cMu dist = (-1) * V.foldl' go 0 dist
  where
    go :: f -> f -> f
    go cMu p = cMu + (p * logBase 2 p)
