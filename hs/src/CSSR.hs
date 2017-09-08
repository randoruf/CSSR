-------------------------------------------------------------------------------
-- |
-- The CSSR Algorithm. This module exemplifies how the components of CSSR
-- connect together into a cohesive algorithm
-------------------------------------------------------------------------------
module CSSR (cssr) where

import CSSR.Algorithm.Phase1 (initialization)
import CSSR.Algorithm.Phase2 (grow)
import CSSR.Algorithm.Phase3 (refine)

cssr :: FilePath -> IO ()
cssr filepath = do
  contents <- readFile filepath
  let histTree = initialization 1 contents
  print histTree
  return ()


