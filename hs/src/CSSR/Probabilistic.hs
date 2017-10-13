{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module CSSR.Probabilistic
  -- classes
  ( Probabilistic(..)
  -- helpers
  , freqToDist
  , matchesFreqs
  , addFrequencies

  -- UNUSED helpers
  -- , distribution
  -- , fsum
  -- , rounded
  -- , matchesDists

  -- UNUSED average adt
  -- , Average(..)
  -- , getAverageI
  -- , getAverageF


  -- re-exports
  , TestResult(..)
  ) where

import CSSR.Prelude

import Control.Monad.Primitive
import Statistics.Test.KolmogorovSmirnov (TestResult(..))

import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as UV
import qualified Statistics.Test.KolmogorovSmirnov as KS


class Probabilistic leaf where
  frequency :: GVector v Integer => leaf -> v Integer

  frequency_ :: GVector v Integer => Monad m => leaf -> m (v Integer)
  frequency_ = pure . frequency

distribution :: (GVector v Integer, GVector v Double, Probabilistic leaf) => leaf -> v Double
distribution = freqToDist . frequency

fsum :: Probabilistic leaf => leaf -> Integer
fsum = V.sum . frequency

rounded :: Probabilistic leaf => leaf -> Vector Float
rounded leaf = V.map (shorten 2) (distribution leaf)
  where
    shorten :: Int -> Double -> Float
    shorten n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

matchesFreqs :: (GVector v Integer, GVector v Double) => Double -> v Integer -> v Integer -> TestResult
matchesFreqs s a b = kstestWrapper s (GV.map fromIntegral a) (GV.map fromIntegral b)

matchesDists :: (GVector v Integer, GVector v Double) => Double -> v Double -> v Double -> TestResult
matchesDists s a b = kstestWrapper s a b

kstestWrapper :: (GVector v Integer, GVector v Double) => Double -> v Double -> v Double -> TestResult
kstestWrapper s a b = KS.kolmogorovSmirnovTest2 s (GV.convert $ a) (GV.convert $ b)

addFrequencies :: (GVector v Integer) => v Integer -> v Integer -> v Integer
addFrequencies = GV.zipWith (+)

freqToDist :: (GVector v Integer, GVector v Double) => v Integer -> v Double
freqToDist fs = GV.map (\f -> fromIntegral f / total) fs
  where
    total :: Double
    total = (fromIntegral . GV.sum) fs

-- unsafeMatch :: (PrimMonad m, (GV.Mutable v m) Integer, GVector v0 Integer) => v Integer -> v0 Integer -> Double -> m Bool
-- unsafeMatch mvec vec sig = do
--   vec' <- GV.basicUnsafeFreeze mvec
--   return $ kstwoTest_ vec' vec sig
--
--
-- unsafeMatch_ :: PrimMonad m
--             => GVector v Integer
--             => v Integer
--             -> v Integer -> Double -> m Bool
-- unsafeMatch_ mvec0 mvec1 sig = do
--   vec0 <- GV.basicUnsafeFreeze mvec0
--   vec1 <- GV.basicUnsafeFreeze mvec1
--   return $ kstwoTest_ vec0 vec1 sig
--

-- test :: Probabilistic inst => inst -> inst -> Double -> Bool
-- test state testCase sig = nullHypothesis state testCase >= sig
--
--
-- nullHypothesis :: (Probabilistic empirical, Probabilistic test)
--                => empirical -> test -> Double
-- nullHypothesis ss val = kstwo (countsAndDist ss) (countsAndDist val)
--   where
--     countsAndDist :: Probabilistic p => p -> (Integer, Vector Double)
--     countsAndDist = totalCounts &&& distribution



-- ========================================================================= --

newtype Average a = Average { unAverage :: (Int, a) }

instance Num a => Monoid (Average a) where
  mempty = Average (0, 0)
  mappend (Average (c0, a0)) (Average (c1, a1)) = Average (c0+c1, a0+a1)

-- TODO: this might throw an Arithmetic error
getAverageI :: (Fractional a, Integral n) => Average n -> a
getAverageI (Average (l, s)) = fromIntegral s / fromIntegral l

-- TODO: this might throw an Arithmetic error
getAverageF :: (Fractional a) => Average a -> a
getAverageF (Average (l, s)) = s / fromIntegral l

