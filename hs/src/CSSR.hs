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
import Data.Tree.Hist as H
import Control.Monad.ST


cssr :: FilePath -> IO ()
cssr filepath = do
  contents <- readFile filepath
  let
    htree = initialization 1 contents
    ltree = runST $ do
      lt' <- grow 0.01 htree
      refine (H.alphabet htree) lt'
      ML.freezeTree lt'

  print htree
  -- print ltree
  return ()



