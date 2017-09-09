module CSSR.Probabilistic where

import CSSR.Prelude
import CSSR.Statistics.KologorovSmirnov

import qualified Data.Vector as V
import qualified Data.Vector.Generic as MV
import Control.Monad.Primitive

class Probabilistic leaf where
  frequency :: leaf -> Vector Integer

  frequency_ :: Monad m => leaf -> m (Vector Integer)
  frequency_ = pure . frequency

distribution :: Probabilistic leaf => leaf -> Vector Double
distribution = freqToDist . frequency

totalCounts :: Probabilistic leaf => leaf -> Integer
totalCounts a = foldr (+) 0 $ frequency a

rounded :: Probabilistic leaf => leaf -> Vector Float
rounded leaf = V.map (shorten 2) (distribution leaf)
  where
    shorten :: Int -> Double -> Float
    shorten n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

matches :: (Probabilistic a, Probabilistic b) => a -> b -> Double -> Bool
matches a b = matchesDists (probabilisticToTuple a) (probabilisticToTuple b)

probabilisticToTuple :: Probabilistic a => a -> (Integer, Vector Double)
probabilisticToTuple a = (sum $ frequency a, distribution a)

matchesDists :: (Integer, Vector Double) -> (Integer, Vector Double) -> Double -> Bool
matchesDists = kstwoTest

matchesDists_ :: Vector Integer -> Vector Integer -> Double -> Bool
matchesDists_ = kstwoTest_

addFrequencies :: Vector Integer -> Vector Integer -> Vector Integer
addFrequencies = V.zipWith (+)

freqToDist :: Vector Integer -> Vector Double
freqToDist fs = V.map (\f -> fromIntegral f / total) fs
  where
    total :: Double
    total = (fromIntegral . sum) fs

unsafeMatch :: PrimMonad m
            => V.MVector (PrimState m) Integer -> Vector Integer -> Double -> m Bool
unsafeMatch mvec vec sig = do
  vec' <- MV.basicUnsafeFreeze mvec
  return $ kstwoTest_ vec' vec sig


unsafeMatch_ :: PrimMonad m
            => V.MVector (PrimState m) Integer
            -> V.MVector (PrimState m) Integer -> Double -> m Bool
unsafeMatch_ mvec0 mvec1 sig = do
  vec0 <- MV.basicUnsafeFreeze mvec0
  vec1 <- MV.basicUnsafeFreeze mvec1
  return $ kstwoTest_ vec0 vec1 sig


test :: Probabilistic inst => inst -> inst -> Double -> Bool
test state testCase sig = nullHypothesis state testCase >= sig


nullHypothesis :: (Probabilistic empirical, Probabilistic test)
               => empirical -> test -> Double
nullHypothesis ss val = kstwo (countsAndDist ss) (countsAndDist val)
  where
    countsAndDist :: Probabilistic p => p -> (Integer, Vector Double)
    countsAndDist = totalCounts &&& distribution



