{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module CSSR.Probabilistic
  -- classes
  ( Probabilistic(..)

  , freqToDist
  , addFrequencies

  , isSameDist
  , isSameDist'
  , isDifferentDist
  , isDifferentDist'

  -- re-exports
  , TestResult(..)
  ) where

import CSSR.Prelude hiding (sort)

import Control.Monad.Primitive
import Statistics.Function (sort)
import Statistics.Test.Types (TestResult(..), significant)
import Statistics.Types (Sample)

import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed as U
import qualified Statistics.Test.KolmogorovSmirnov as KS


class Probabilistic leaf where
  frequency :: GVector v Integer => leaf -> v Integer

  frequency_ :: GVector v Integer => Monad m => leaf -> m (v Integer)
  frequency_ = pure . frequency

distribution :: (GVector v Integer, GVector v Double, Probabilistic leaf) => leaf -> v Double
distribution = freqToDist . frequency

addFrequencies :: (GVector v Integer) => v Integer -> v Integer -> v Integer
addFrequencies = GV.zipWith (+)

freqToDist :: (GVector v Integer, GVector v Double) => v Integer -> v Double
freqToDist fs = GV.map ((/ total) . fromIntegral) fs
  where
    total :: Double
    total = (fromIntegral . GV.sum) fs

-- ========================================================================= --

isSameDist' :: Double -> Vector Integer -> Vector Integer -> Bool
isSameDist' a b c = not (isDifferentDist' a b c)

isSameDist :: Double -> Vector Double -> Integer -> Vector Double -> Integer -> Bool
isSameDist a b c d e = not (isDifferentDist a b c d e)

isDifferentDist' :: Double -> Vector Integer -> Vector Integer -> Bool
isDifferentDist' alpha f0 f1 = isDifferentDist alpha (freqToDist f0) (V.sum f0) (freqToDist f1) (V.sum f1)

-- The null hypothesis is that the two samples come from the same distribution.
isDifferentDist :: Double -> Vector Double -> Integer -> Vector Double -> Integer -> Bool
isDifferentDist alpha d0 (fromIntegral->n) d1 (fromIntegral->m)
  = ksstatistic d0 d1 >= c alpha * sqrt ((n + m) / (n * m))
 where
  -- https://www.webdepot.umontreal.ca/Usagers/angers/MonDepotPublic/STT3500H10/Critical_KS.pdf
  c :: Double -> Double
  c = \case
    0.100 -> 1.22
    0.050 -> 1.36
    0.025 -> 1.48
    0.010 -> 1.63
    0.005 -> 1.73
    0.001 -> 1.95
    a     -> sqrt $ (-0.5) * log (a / 2)


-- |
-- calculate the Kolmogorov-Smirnov statistic
--
-- @param data1  sample one's pdf, used to calculate the first ecdf
-- @param data2  sample two's pdf, used to calculate the second ecdf
-- @return       the KS statistic: the supremum of the tow calculated ecdfs
ksstatistic :: Vector Double -> Vector Double -> Double
ksstatistic data1 data2 =
  -- assert (length data1 == length data2)
  -- calc empirical cumulative distributions. Should have ascending order.
  maximum $ V.zipWith (\ a b -> abs $ a - b) ecdf1 ecdf2
 where
  ecdf1 = V.scanl' (+) 0 data1
  ecdf2 = V.scanl' (+) 0 data2


