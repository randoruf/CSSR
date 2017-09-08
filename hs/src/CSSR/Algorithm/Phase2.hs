-------------------------------------------------------------------------------
-- | == Phase II: "Growing a Looping Tree" algorithm
--
-- INIT root looping node
-- INIT queue of active, unchecked nodes
-- QUEUE root
-- WHILE queue is not empty
--   DEQUEUE first looping node from the queue
--   COMPUTE homogeneity(dequeued looping node, parse tree)
--   IF node is homogeneous
--   THEN continue
--   ELSE
--     CONSTRUCT new looping nodes for all valid children (one for each symbol in
--               alphabet - must have empirical observation in dataset).
--     FOR each new node constructed
--       COMPUTE excisability(node, looping tree)
--       COMPUTE isEdge(node, looping tree)
--       ADD all new looping nodes to children of active node (mapped by symbol)
--       ADD unexcisable children to queue (FIXME: what about edgesets?)
--   ENDIF
-- ENDWHILE
--
-- isEdge:
--   INPUTS: looping node, looping tree
--   COLLECT all terminal nodes that are not ancestors
--   IF exists terminal nodes with identical distributions
--   THEN
--     mark looping node as an edge set
--     mark found terminals as an edge set
--     // We will merge edgesets in Phase III.
--   ENDIF
--
-------------------------------------------------------------------------------
module CSSR.Algorithm.Phase2 where

import qualified Data.Tree.Looping as Looping (Tree)
import qualified Data.Vector as V
import qualified Data.MTree.Parse as M
import qualified Data.Tree.Parse as P
import qualified Data.Tree.Hist as Hist
import qualified Data.MTree.Parse as MHist
import Debug.Trace


grow :: Hist.Tree -> Looping.Tree
grow htree = go (root htree) (mkActiveQueue htree)
  where
    root = undefined
    mkActiveQueue = undefined
    go = undefined


