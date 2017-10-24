module CSSR.Statistics.Variation where

import CSSR.Prelude
import qualified Data.Tree.Conditional as Cond
import qualified Data.Vector as V

type InferredDistribution = [(Cond.Leaf, Double)]

variation :: InferredDistribution -> Double -> Double
variation dist adjustedDataSize = foldl' go 0 dist
  where
    go :: Double -> (Cond.Leaf, Double) -> Double
    go total (h, inferredProb) = total + abs (historyProb - inferredProb)
      where
        historyProb :: Double
        historyProb = V.sum $ V.map ((/ adjustedDataSize).fromIntegral) (view Cond.lfrequencyL h)
