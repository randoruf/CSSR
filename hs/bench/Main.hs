{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Criterion (Benchmark)
import Criterion.Main (defaultMain, env, nf, bench)
import Control.DeepSeq (NFData)
import Control.Exception ()

import CSSR.Prelude (Event)
import qualified Data.Vector as V
import qualified Data.MTree.Parse as MParse
import qualified Data.Tree.Parse  as Parse

main :: IO ()
main = defaultMain $ fmap tester [10000] -- [500,1000..10000]
  where
    test :: Int -> V.Vector Event -> Benchmark
    test c vec = bench ("buildTree " ++ show c) $ nf (MParse.buildTree 10) vec

    tester :: Int -> Benchmark
    tester c = env (pure $ V.replicate c "1") (test c)


instance NFData Parse.Tree
instance NFData Parse.Leaf
instance NFData Parse.LeafBody

