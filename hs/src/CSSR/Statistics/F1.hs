{-# OPTIONS_GHC -Wno-type-defaults #-}
module CSSR.Statistics.F1 where

import CSSR.Prelude

f1 :: Foldable f => (Double -> Bool) -> f Double -> f Double -> Double
f1 = fBeta 1

fBeta :: Foldable f => Double -> (Double -> Bool) -> f Double -> f Double -> Double
fBeta beta isPositive truth predict = (1 + beta ^ 2) * (p * r) / ((beta ^ 2) * p + r)
  where
    p, r :: Double
    p = precision isPositive truth predict
    r = recall    isPositive truth predict


precision :: Foldable f => (Double -> Bool) -> f Double -> f Double -> Double
precision isPositive truth predict = tp / (tp + fp)
  where
    tp, fp :: Double
    tp = truePositives  isPositive truth predict
    fp = falsePositives isPositive truth predict

recall :: Foldable f => (Double -> Bool) -> f Double -> f Double -> Double
recall isPositive truth predict = tp / (tp + fn)
  where
    tp, fn :: Double
    tp = truePositives  isPositive truth predict
    fn = falseNegatives isPositive truth predict

-- helpers

countWhere :: Foldable f => (Double -> Double -> Bool) -> f Double -> f Double -> Double
countWhere fn truth predict = fromIntegral . sum $
  zipWith (\t p -> fromEnum (fn t p)) (toList truth) (toList predict)

truePositives :: Foldable f => (Double -> Bool) -> f Double -> f Double -> Double
truePositives isPositive = countWhere (\t p -> isPositive t && t == p)

falsePositives :: Foldable f => (Double -> Bool) -> f Double -> f Double -> Double
falsePositives isPositive = countWhere (\t p -> not (isPositive t) && t /= p)

falseNegatives :: Foldable f => (Double -> Bool) -> f Double -> f Double -> Double
falseNegatives isPositive = countWhere (\t p -> isPositive t && t /= p)





