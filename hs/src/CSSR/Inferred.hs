{-# LANGUAGE NamedFieldPuns #-}
module CSSR.Inferred where

import Data.CSSR.State
import CSSR.Prelude
import Data.Tree.Conditional (lobsL)

import qualified Data.Tree.Conditional as Cond
import qualified Data.Tree.Looping as L
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

type LLeaf = L.Leaf

-- | calculates the probability of all the histories up to length/depth indicated, based
-- on a given allStates
inferredDistribution :: Cond.Tree -> Int -> AllStates -> [(Cond.Leaf, Double)]
inferredDistribution tree@Cond.Tree{Cond.depth} d allstates =
  fmap (identity &&& inferredProb tree allstates . view lobsL)
    (tree `Cond.getDepth` depth)

-- | calculates the probability of a single, raw history (in string form) based on a given
-- allStates and alphabet
inferredProb :: Cond.Tree -> HashSet State -> Vector Event -> Double
inferredProb tree allstates hist = sum . toList . HS.map totalPerString $ allstates
 where
  dist :: State -> HashMap Event Double
  dist = distributionsLookup (Cond.alphabet tree) allstates

  allStatesDist :: State -> Double
  allStatesDist = distributionLookup (Cond.alphabet tree) allstates

  totalPerString :: State -> Double
  totalPerString start = fst (V.foldl' go (1, start) hist) * allStatesDist start
   where
    go :: (Double, State) -> Event -> (Double, State)
    go (charTotalPerState, current) e =
      case current `transitionNonNull` e of
        -- FIXME: because there exists a null states within misuriwicz
        -- (history lengths are too long?), this is currently destroying results
        Nothing -> (0, current)
        Just t  -> (HM.lookupDefault 0 e (dist current) * charTotalPerState, t)

    transitionNonNull :: State -> Event -> Maybe State
    transitionNonNull s e = do
      t <- s `transitionTo` e
      guard (isRoot s)
      pure t

    {-
    // TODO: ask about this scala code: seems like we are just hitting a steady-state on every
    // history. is this normal? if so, it looks like we are double-ish counting.
    leaf.observed.reverse.view.zipWithIndex.foldLeft[Double](1d){
      (totalPerState, pair) => {
        val (c, i) = pair

        val currentState = current.get.currentEquivalenceClass
        val next = current.get.findChildWithAdditionalHistory(c)
        val nextEqClassIdx = allStates.states.indexOf(next.get.currentEquivalenceClass)

        current = next
        if (!allStates.states.contains(next.get.currentEquivalenceClass)) {
          0d // we let this 0-probability eliminate null states.
        } else {
          totalPerState * currentState.distribution(alphabet.map(c))
        }
      }
    }
    -}

