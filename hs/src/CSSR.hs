-------------------------------------------------------------------------------
-- |
-- The CSSR Algorithm. This module exemplifies how the components of CSSR
-- connect together into a cohesive algorithm
-------------------------------------------------------------------------------
module CSSR (cssr) where

import CSSR.Prelude
import CSSR.Algorithm.Phase1 (initialization)
import CSSR.Algorithm.Phase2 (grow)
import CSSR.Algorithm.Phase3 (refine)
import Data.MTree.Looping as ML
import Data.Tree.Looping as L
import Data.Tree.Conditional as Cond
import Control.Monad.ST


cssr :: Double -> Int -> FilePath -> IO ()
cssr sig d filepath = do
  contents <- readFile filepath
  let
    htree = initialization d contents
    ltree = runST $ do
      lt' <- grow sig htree
      refine (Cond.alphabet htree) lt'
      ML.freezeTree lt'

  print htree
  print ltree
  return ()

defaultCSSR :: FilePath -> IO ()
defaultCSSR = cssr 0.01 5


