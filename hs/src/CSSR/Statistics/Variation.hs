module CSSR.Statistics.Variation where

import CSSR.Prelude
import qualified Data.Tree.Hist as Hist
import qualified Data.Vector as V

type InferredDistribution = [(Hist.Leaf, Double)]

variation :: InferredDistribution -> Double -> Double
variation dist adjustedDataSize = foldl' go 0 dist
  where
    go :: Double -> (Hist.Leaf, Double) -> Double
    go total (h, inferredProb) = total + abs (historyProb - inferredProb)
      where
        historyProb :: Double
        historyProb = V.sum $ V.map ((/ adjustedDataSize).fromIntegral) (view Hist.lfrequencyL h)
