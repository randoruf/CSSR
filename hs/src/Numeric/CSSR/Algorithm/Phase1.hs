-------------------------------------------------------------------------------
-- |
-- == Phase I: "Initialization"
--
-- Requires estimates of conditional probabilities to converge, perhaps rapidly.
--
-------------------------------------------------------------------------------
module Numeric.CSSR.Algorithm.Phase1 where

import CSSR.Prelude
import qualified Data.Text    as T (singleton)
import qualified Data.Vector  as V (fromList)

import qualified Data.MTree.Parse   as M (buildTree)
import qualified Data.Tree.Parse    as P (Tree)
import qualified Data.Tree.Conditional as Cond (Tree, convert)


initialization :: Int -> [Char] -> Cond.Tree
initialization depth (fmap T.singleton -> s) = Cond.convert parseTree
  where
    parseTree :: P.Tree
    parseTree = M.buildTree depth . V.fromList $ s
