{-# OPTIONS_GHC -Wno-unused-matches #-} -- FIXME: remove when no more undefineds are left
module CSSR.Statistics.EntropyRate where

import CSSR.Prelude
import qualified Data.HashMap.Strict as HM

type TransitionMap = HashMap State (HashMap Event (Maybe State))
type State = ()
type Symbol = Text

distribution :: State -> HashMap Symbol Double
distribution = undefined

entropyRate :: TransitionMap -> Double
entropyRate ts =
  (-1) * sum (go <$> HM.keys ts)
  where
    go :: State -> Double
    go s = foldl' go' 0 (HM.toList $ distribution s)

    go' :: Double -> (Symbol, Double) -> Double
    go' sEntRate (s, p)
      -- technically, we can remove branching, but I don't know what scala will do, given log(0)
      | p > 0  = sEntRate + freq * p * log2 p
      -- Also note that scala logs are the natural log
      | p <= 0 = sEntRate
      where freq = undefined
            log2 = undefined
{-
entropyRate :: [(ProbabilityDistribution, FrequencyDistribution)] -> Double
entropyRate = ((-1) *) . sum . map stateER
  where
    stateER :: (ProbabilityDistribution, FrequencyDistribution) -> Double
    stateER (ps, fs) = foldr go 0 (zip ps fs)

    go :: Double -> (Probability, Frequency) -> Double
    go entr (p, f) = if p <= 0 then entr else entr + f * p * log p

discreteEntropy :: Double -> Double -> Double
discreteEntropy a b = a * log (a / b)

data ParseLeaf
  = ParseLeaf
  { totalCounts :: Integer
  } deriving (Show)

type InferredDistribution = [(ParseLeaf, Double)]

-- |
-- calculates the probability of all the max length strings in the data based on
-- the inferred machine.
relativeEntropy
  :: InferredDistribution -- ^ dist the inferred distribution of the *histories of max length*.
  -> Integer -- ^ data size
  -> Double
relativeEntropy dist dataSize = undefined -- if relEnt < 0 then 0 else relEnt
  where
    inferredPs = map snd dist
    observedPs = map (nonZero . toP . totalCounts . fst) dist

    toP :: Integer -> Probability
    toP counts = ((/) `on` fromIntegral) counts dataSize

    nonZero :: Double -> Double
    nonZero p = if p < 0 then 0 else p -- paranoia from original C version

    go :: Double -> (ParseLeaf, Double) -> Double
    go ent (leaf, infP) =
      let
        p = undefined -- obsP leaf
      in
        ent + (if p > 0 then discreteEntropy p infP else 0)

--
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
