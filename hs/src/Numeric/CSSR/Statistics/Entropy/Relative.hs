module Numeric.CSSR.Statistics.Entropy.Relative where

import CSSR.Prelude
import qualified Data.Tree.Conditional as Cond

import Numeric.CSSR.Internal

-- calculates the probability of all the max length strings in the
-- data based on the inferred machine:
--
-- Kullback-Leibler Distance:
--
-- \begin{equation}
--    d= \sum_{k} p_k * \log_2 * { p_k \over q_k }
-- \end{equation}
--
-- @param dist the inferred distribution of the *histories of max length*.
-- @param adjustedDataSize
-- @return
relativeEntropy :: InferredDistribution -> Double -> Double
relativeEntropy dist adjustedDataSize =
  -- FIXME : this _should not be <0_ however calculations provide the contrary
  -- when the generated, inferred probability is greater than the observed one - we find the added log-ratio is < 0
  -- currently, we have too many of that for the even process. This also begs the question: should there _ever_ be
  -- a calculation where the log-ratio is < 0, since it was permissible in the C++. It may be that this is not the case
  -- since I believe we are using K-L distribution for conditional, empirical distributions (yes?)
  if relent < 0 then 0 else relent
  where
    relent :: Double
    relent = foldl go 0 dist

    go :: Double -> (Cond.Leaf, Double) -> Double
    go incrementalRelEnt (leaf, inferredProb) =
      case compare observedProb 0 of
        -- it seems to me that we should be checking if the inferred probability is > 0.
        -- By virtue of this: should the conditional be flipped? Note: this makes the final rel entropy non-negative
        --   if (inferredProb > 0){
        --     val logRatio = math.log(inferredProb / observedProb) // note that math.log in scala is the natural log
        --     val cacheRE = incrementalRelEnt + inferredProb * logRatio
        GT -> incrementalRelEnt + discreteEntropy observedProb inferredProb
        _  -> trace ("NO AGGREGATION! dataProb: " <> show observedProb) incrementalRelEnt
      where
        observedProb :: Double
        observedProb = (fromIntegral . sum $ view Cond.lfrequencyL leaf) / adjustedDataSize

{-
-- Kullback-Leibler Distance:
--
-- \begin{equation}
--    d= \sum_{k} p_k * \log_2 * { p_k \over q_k }
-- \end{equation}
--
klDist
  :: [Probability] -- ^ observed probabilities
  -> [Probability] -- ^ inferred probabilities
  -> Double
klDist = sum (uncurry . discreteEntropy) . zip

  def entropyRate(allStates: AllStates):Double = {
    (-1) * allStates.states
      .view
      .zipWithIndex
      .map {
        case (state, i) =>
          val freq = allStates.distribution(i)

          state.distribution.foldLeft(0d) {
            // technically, we can remove branching, but I don't know what scala will do, given log(0)
            case (stateEntRate, prob) if prob > 0 => stateEntRate + (freq * (prob * log2(prob)))
            // Also note that scala logs are the natural log
            case (         ser, prob) if prob <=0 => ser
          } }
      .sum[Double]
-}
