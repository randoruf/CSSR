module Numeric.CSSR.Inferred where

import Protolude hiding (State)
import Control.Arrow ((&&&))
import Data.Vector (Vector)
import Data.HashMap.Strict (HashMap)
import Lens.Micro.Platform (view)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Data.CSSR.Alphabet
import Data.CSSR.State
import qualified Data.Tree.Conditional as Cond

type InferredDistribution = Vector (Cond.Leaf, Double)

-- | calculates the probability of all the histories up to length/depth indicated, based on a given allStates
inferredDistribution :: Alphabet -> Cond.Tree -> Int -> AllStates -> InferredDistribution
inferredDistribution a t depth ss =
  V.fromList $ map (identity &&& go) (Cond.getDepth t depth)
 where
  go :: Cond.Leaf -> Double
  go = inferredHistory a t (asVec ss) . view Cond.lobsL

-- | calculates the probability of a single, raw history (in string form) based on a given allStates and alphabet
inferredHistory :: Alphabet -> Cond.Tree -> Vector State -> Vector Event -> Double
inferredHistory a tree allStates history = totalPerString
 where
  totalPerString :: Double
  totalPerString = V.sum $ V.imap go allStates
    where
      go :: Int -> State -> Double
      go i s = distributionLookup a allStates s * fromMaybe 0 (V.foldl (go' s) (Just 1) history)

      -- FIXME: because there exists a null states within misuriwicz (history lengths are too long?)
      -- this is currently destroying results
      go' :: State -> Maybe Double -> Event -> Maybe Double
      go' _                       Nothing _ = Nothing
      go' s (Just characterTotalPerState) c =
        case transitionState of
          Nothing -> Nothing
          Just t  -> Just $ characterTotalPerState * evtStateProb c
        where
          ts :: HashMap Event (Maybe State)
          ts = allTransitionsLookup a allStates s

          transitionState :: Maybe State
          transitionState = join $ HM.lookup c ts

          stateDist ::  HashMap Event Double
          stateDist = distributionsLookup a allStates s

          evtStateProb :: Event -> Double
          evtStateProb evt = HM.lookupDefault
            (panic "every event is accounted for / OR THIS SHOULD BE ZERO")
            evt stateDist

    --  TODO: ask about this: seems like we are just hitting a steady-state on every history. is this normal? if so, it looks like we are double-ish counting.
    -- af.observed.reverse.view.zipWithIndex.foldLeft[Double](1d){
    -- (totalPerState, pair) => {
    --   val (c, i) = pair
    --   val currentState = current.get.currentEquivalenceClass
    --   val next = current.get.findChildWithAdditionalHistory(c)
    --   val nextEqClassIdx = allStates.states.indexOf(next.get.currentEquivalenceClass)
    --   println(totalPerState, c, current.get.observed, next.get.observed, leaf.observed, nextEqClassIdx)
    --   current = next
    --   if (!allStates.states.contains(next.get.currentEquivalenceClass)) {
    --     0d // we let this 0-probability eliminate null states.
    --   } else {
    --     totalPerState * currentState.distribution(alphabet.map(c))
    --   }
    -- }

