{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Numeric.CSSR.Statistics.Entropy.Relative.Rate where

import Protolude hiding (State)
import Data.Vector (Vector)
import Data.Function (on)
import Lens.Micro.Platform (view)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Data.CSSR.State
import Data.CSSR.Alphabet
import Numeric.CSSR.Inferred
import qualified Data.Tree.Conditional as Cond

relativeEntropyRate
  :: Alphabet
  -> Integer
  -> InferredDistribution
  -> Cond.Tree
  -> AllStates
  -> Double
relativeEntropyRate as adjustedDataSize maxLengthDist tree allStates =
  foldl go 0 nextLastHistoryDist
 where
  nextLastHistoryDist :: InferredDistribution
  nextLastHistoryDist = inferredDistribution as tree (Cond.depth tree - 1) allStates

  go :: Double -> (Cond.Leaf, Double) -> Double
  go partialRelEntRate (l, inferredP)
    = partialRelEntRate
    + relEntropyRateForHistory as adjustedDataSize l inferredP tree allStates

newtype NonZero a = NonZero { unNonZero :: a }
  deriving (Show, Eq, Num)

getNonZero :: (Eq a, Num a) => NonZero a -> Maybe a
getNonZero (NonZero a)
  | a == 0    = Nothing
  | otherwise = Just a

newtype Positive a = Positive { unPositive :: a }
  deriving (Show, Eq, Num)

getPositive :: (Ord a, Num a) => Positive a -> Maybe a
getPositive (Positive a)
  | a < 0     = Nothing
  | otherwise = Just a

-- the frequency of occurrence of the history with that particular alpha symbol
relEntropyRateForHistory
  :: Alphabet
  -> Integer
  -> Cond.Leaf
  -> Double
  -> Cond.Tree
  -> AllStates
  -> Double
relEntropyRateForHistory alpha adjustedDataSize history inferredProb tree allStates =
  maybe 0 (* histProbability) (getPositive relEntRateHistTotal)
 where
  relEntRateHistTotal :: Positive Double
  relEntRateHistTotal = foldl go 0 (idxToSym alpha)

  aIx :: Event -> Int
  aIx e = HM.lookupDefault (panic "unknown symbol") e (symToIdx alpha)

  (//) :: Integer -> Integer -> Double
  (//) = (/) `on` fromIntegral 

  histProbability :: Double
  histProbability = Cond.total history // adjustedDataSize

  go :: Positive Double -> Event -> Positive Double
  go relEntRateHist a = relEntRateHist + Positive relEntRateAlpha
    where
      hfreq :: Vector Integer
      hfreq = view Cond.lfrequencyL history

      histFreqByAlpha :: Double
      histFreqByAlpha = hfreq V.! aIx a // V.sum hfreq

      minferredProb = NonZero (Positive inferredProb)
      mhistFreqByAlpha = NonZero (Positive histFreqByAlpha)

      (inferredRatio, relEntRateAlpha) =
        if inferredProb <= 0
        then (0, 0)
        else relEntropyRateByNextAlphabet
               alpha
               (view Cond.lobsL history)
               minferredProb
               tree
               allStates
               mhistFreqByAlpha
               a


mRelEntropyRateByNextAlphabet
  :: Alphabet
  -> Vector Event
  -> Maybe (Positive Double)
  -> Cond.Tree
  -> AllStates
  -> Maybe (NonZero (Positive Double))
  -> Event
  -> (Double, Double)
mRelEntropyRateByNextAlphabet as history minferredProb tree allStates mhistFreqByAlpha a =
  case (mhistFreqByAlpha, minferredProb) of
    (Just _, Just 0) -> (0, 0)
    (Just f, Just i) -> relEntropyRateByNextAlphabet as history (NonZero i) tree allStates f a
    (     _,      _) -> panic "FIXME: Something disastrous just happened."

relEntropyRateByNextAlphabet
  :: Alphabet
  -> Vector Event
  -> NonZero (Positive Double)
  -> Cond.Tree
  -> AllStates
  -> NonZero (Positive Double)
  -> Event
  -> (Double, Double)
relEntropyRateByNextAlphabet as history inferredProb tree allStates histFreqByAlpha a =
  (inferredRatio, relEntRateAlpha)

 where
  childStringProb :: Double
  childStringProb = inferredHistory as tree (asVec allStates) (a `V.cons` history)

  inferredRatio :: Double
  inferredRatio = childStringProb / (get inferredProb)

  relEntRateAlpha :: Double
  relEntRateAlpha = discreteEntropy (get histFreqByAlpha) inferredRatio

  get :: NonZero (Positive Double) -> Double
  get = unPositive . unNonZero

discreteEntropy :: Double -> Double -> Double
discreteEntropy a b = a * logBase 2 (a / b)

{-
  // the frequency of occurrence of the history with that particular alpha symbol
  protected def relEntropyRateForHistory(history: Cond.Leaf, inferredProb:Double, tree: ParseTree, allStates: AllStates):Double = {
    debug(s"stringProb: $inferredProb, for history: ${history.toString}")

    val relEntRateHistTotal:Double = tree.alphabet
      .raw
      .foldLeft(0d){
        (relEntRateHist, alpha) => {
          val histFreqByAlpha = history.frequency(tree.alphabet.map(alpha)) / history.totalCounts
          val (inferredRatio, relEntRateAlpha) = relEntropyRateByNextAlphabet(history.observed, inferredProb, tree, allStates, histFreqByAlpha, alpha)

          debug(s"relEntRateAlpha: $relEntRateAlpha")
          debug(s"relEntRateHist: ${relEntRateHist + relEntRateAlpha}")

          relEntRateHist + relEntRateAlpha
        }
      }

    val histProbability:Double = history.totalCounts / tree.adjustedDataSize

    debug(s"histFrequency: $histProbability")

    if (relEntRateHistTotal < 0) 0 else relEntRateHistTotal * histProbability
  }

-}
