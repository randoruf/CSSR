{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module CSSR.Probabilistic
  -- classes
  ( Probabilistic(..)
  -- helpers
  , freqToDist
  , matchesFreqs
  , addFrequencies
  , matchesFreqsAsDists

  -- UNUSED helpers
  -- , distribution
  -- , fsum
  -- , rounded
  , matches
  , matchesDists
  , isSameDist
  , isSameDist'

  -- UNUSED average adt
  -- , Average(..)
  -- , getAverageI
  -- , getAverageF


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

fsum :: Probabilistic leaf => leaf -> Integer
fsum = V.sum . frequency

rounded :: Probabilistic leaf => leaf -> Vector Float
rounded leaf = V.map (shorten 2) (distribution leaf)
  where
    shorten :: Int -> Double -> Float
    shorten n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

matches :: (GVector v Integer, GVector v Double) => Double -> v Integer -> v Integer -> TestResult
matches s a b = kolmogorovSmirnovTest2 s
  (GV.convert (freqToDist a), GV.sum a)
  (GV.convert (freqToDist b), GV.sum b)

matchesFreqs :: (GVector v Integer, GVector v Double) => Double -> v Integer -> v Integer -> TestResult
matchesFreqs s a b = kstestWrapper s (GV.map fromIntegral a) (GV.map fromIntegral b)

matchesFreqsAsDists :: (GVector v Integer, GVector v Double) => Double -> v Integer -> v Integer -> TestResult
matchesFreqsAsDists s a b = kstestWrapper s (freqToDist $ GV.map fromIntegral a) (freqToDist $ GV.map fromIntegral b)

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

-- ========================================================================= --

-- | Two sample Kolmogorov-Smirnov test. It tests whether two data
--   samples could be described by the same distribution without
--   making any assumptions about it.
--
--   This test uses approxmate formula for computing p-value.
kolmogorovSmirnovTest2 :: Double -- ^ p-value
                       -> (Sample, Integer) -- ^ Sample 1
                       -> (Sample, Integer) -- ^ Sample 2
                       -> TestResult
kolmogorovSmirnovTest2 p (xs1, n1') (xs2, n2')
  | p > 0 && p < 1 = significant $ 1 - prob( d*(en + 0.12 + 0.11/en) ) < p
  | otherwise      = error "Statistics.Test.KolmogorovSmirnov.kolmogorovSmirnovTest2:bad p-value"
  where
    d    = kolmogorovSmirnov2D (xs1, n1') (xs2, n2')
    -- Effective number of data points
    n1   = fromIntegral n1'
    n2   = fromIntegral n2'
    en   = sqrt $ n1 * n2 / (n1 + n2)
    --
    prob z
      | z <  0    = error "kolmogorovSmirnov2D: internal error"
      | z == 0    = 1
      | z <  1.18 = let y = exp( -1.23370055013616983 / (z*z) )
                    in  2.25675833419102515 * sqrt( -log y  ) * (y + y**9 + y**25 + y**49)
      | otherwise = let x = exp(-2 * z * z)
                    in  1 - 2*(x - x**4 + x**9)
-- FIXME: Find source for approximation for D

-- | Calculate Kolmogorov's statistic /D/ for two data samples. If
--   either of samples is empty returns 0.
kolmogorovSmirnov2D :: (Sample, Integer) -- ^ First sample
                    -> (Sample, Integer)   -- ^ Second sample
                    -> Double
kolmogorovSmirnov2D (sample1, n1') (sample2, n2')
  | U.null sample1 || U.null sample2 = 0
  | otherwise                        = worker 0 0 0
  where
    xs1 = sort sample1
    xs2 = sort sample2
    n1  = fromIntegral n1'
    n2  = fromIntegral n2'
    en1 = fromIntegral n1
    en2 = fromIntegral n2
    -- Find new index
    skip x i xs = go (i+1)
      where go n | n >= U.length xs = n
                 | xs U.! n == x    = go (n+1)
                 | otherwise        = n
    -- Main loop
    worker d i1 i2
      | i1 >= n1 || i2 >= n2 = d
      | otherwise            = worker d' i1' i2'
      where
        d1  = xs1 U.! i1
        d2  = xs2 U.! i2
        i1' | d1 <= d2  = skip d1 i1 xs1
            | otherwise = i1
        i2' | d2 <= d1  = skip d2 i2 xs2
            | otherwise = i2
        d'  = max d (abs $ fromIntegral i1' / en1 - fromIntegral i2' / en2)

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
  c :: Double -> Double
  c = \case
    0.100 -> 1.22
    0.050 -> 1.36
    0.025 -> 1.48
    0.010 -> 1.63
    0.005 -> 1.73
    0.001 -> 1.95
    a     -> sqrt $ (-0.5) * log (a / 2)
  -- https://www.webdepot.umontreal.ca/Usagers/angers/MonDepotPublic/STT3500H10/Critical_KS.pdf


-------------------------------------------------------------------------------

kstwo' :: Vector Integer -> Vector Integer -> Double
kstwo' a0 a1  = kstwo (freqToDist a0) (V.sum a0) (freqToDist a1) (V.sum a1)

kstwo :: Vector Double -> Integer -> Vector Double -> Integer -> Double
kstwo data1 (fromIntegral->n1) data2 (fromIntegral->n2) =
  -- calculate P(D > observed)
  probks (en + 0.12 + (0.11 / en)) * ksstatistic data1 data2
 where
  en :: Double
  en = sqrt $ (n1 * n2) / (n1 + n2)


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


-- Kolmogorov-Smirnov probability function, Q_{ks}
--
-- Q_{ks}(\lambda) = 2 * \sum_{j=1,\infinity}(-1)^{j-1} e^{-2j^{2}\lamda^{2}}
--
-- This particular calculation assumes that we are working with discreteness
-- across the natural numbers.
--
-- @param alam stands for "a lambda", I believe
-- @return     probability or 1.0 if `probks` fails to converge.
probks :: Double -> Double
probks alam = either identity (const 1.0) $   -- Get here only by failing to converge.
  foldr go (Right (0, 2, 0, 0)) [1..100]
 where
  eps1, eps2, a2 :: Double
  eps1 = 0.001
  eps2 = 1.0e-8
  a2 = -2.0 * alam * alam

  go
    :: Integer
    -> Either Double (Double, Double, Double, Double)
    -> Either Double (Double, Double, Double, Double)
  go _ l@(Left _) = l
  go (fromIntegral->j) (Right (term, fac, sum, termBF)) =
    if (abs term  <= eps1 * termBF) || (abs term' <= eps2 * sum)
    then Left sum'
    else Right (term', fac', sum', abs term')
   where
    term', sum', fac' :: Double
    term' = fac * exp (a2 * j * j)
    sum' = sum + term'
    fac' = -fac              -- Alternating signs in sum.

