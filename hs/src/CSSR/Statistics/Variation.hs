module CSSR.Statistics.Variation where

import CSSR.Prelude
import CSSR.AllStates (State, terminal)
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

