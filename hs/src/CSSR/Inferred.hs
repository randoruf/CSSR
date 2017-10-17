module CSSR.Inferred where

import CSSR.Prelude
import Data.Tree.Hist (lobsL)
import qualified Data.Tree.Hist as Hist

type State = ()
type AllStates = [State]

-- | calculates the probability of all the histories up to length/depth indicated, based on a given allStates
inferredDistribution :: Hist.Tree -> Int -> AllStates -> [(Hist.Leaf, Double)]
inferredDistribution tree d allstates =
  fmap (\ h -> (h, inferredHist (view lobsL h) tree allstates)) $ tree `getDepth` d
  where
    getDepth = undefined

-- | calculates the probability of a single, raw history (in string form) based on a given allStates and alphabet
inferredHist :: Vector Event -> Hist.Tree -> AllStates -> Double
inferredHist hist tree allstates = foldr totalPerString 0 (zip [0..] allstates)
  where
    totalPerString = undefined
