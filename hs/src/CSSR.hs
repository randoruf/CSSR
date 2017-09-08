-------------------------------------------------------------------------------
-- |
-- The CSSR Algorithm. This module exemplifies how the components of CSSR
-- connect together into a cohesive algorithm
-------------------------------------------------------------------------------
module CSSR where

import qualified Data.Tree.Looping as Looping (Tree)
import qualified Data.Vector as V
import qualified Data.MTree.Parse as M
import qualified Data.Tree.Parse as P
import qualified Data.Tree.Hist as Hist
import qualified Data.MTree.Parse as MHist
import Debug.Trace
-- $setup
-- >>> :set -XViewPatterns


main :: FilePath -> IO ()
main filepath = do
  contents <- readFile filepath
  let histTree = initialization 1 contents
  print histTree
  return ()

-------------------------------------------------------------------------------
-- |
-- == Phase I: "Initialization"
--
-- Requires estimates of conditional probabilities to converge, perhaps rapidly.
--
-- FIXME: Currently only works with characters
--
-- Examples:
-- >>> let short_ep = "00011110001100011110000111101101111111111000110001101101100111100111100"
-- >>> initialization 1 short_ep
-- Tree {depth 1, alphabet: ["0","1"]}
--   root:
--      " "->Leaf{obs: [], freq: [28,42]}
--           children:
--           "0"->Leaf{obs: ["0"], freq: [1,1], no children}
-- <BLANKLINE>
--           "1"->Leaf{obs: ["1"], freq: [1,1], no children}
-- >>> initialization 2 short_ep
-- Tree {depth 2, alphabet: ["0","1"]}
--   root:
--      " "->Leaf{obs: [], freq: [28,42]}
--           children:
--           "0"->Leaf{obs: ["0"], freq: [15,12]}
--                children:
--                "0"->Leaf{obs: ["0","0"], freq: [1,1], no children}
-- <BLANKLINE>
--                "1"->Leaf{obs: ["1","0"], freq: [0,1], no children}
-- <BLANKLINE>
--           "1"->Leaf{obs: ["1"], freq: [12,30]}
--                children:
--                "0"->Leaf{obs: ["0","1"], freq: [1,1], no children}
-- <BLANKLINE>
--                "1"->Leaf{obs: ["1","1"], freq: [1,1], no children}
--
-------------------------------------------------------------------------------
initialization :: Int -> [Char] -> Hist.Tree
initialization depth (fmap (:[]) -> s) =
  Hist.convert parseTree $ M.getAlphabet parseTree
  where
    parseTree :: P.Tree
    parseTree = M.buildTree depth . V.fromList $ s

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
grow :: Hist.Tree -> Looping.Tree
grow htree = go (root htree) (mkActiveQueue htree)
  where
    root = undefined
    mkActiveQueue = undefined
    go = undefined

-------------------------------------------------------------------------------
-- | == Phase III: "Refine the Looping Tree"
--
-- SET change = true
-- UNTIL change == false
--   INIT transition map (terminal -> node)
--
--   // calculate predictive probabilities
--   FOR each terminal node (t)
--     FOR each non-looping path w to t
--       FOR each symbol a in the alphabet follow the path wa in the tree
--         IF wa leads to a terminal node
--         THEN store terminal's transition in transition map
--         ELSEIF wa does not lead to a terminal node
--           copy the sub-looping-tree rooted at (the node reached by) wa to t,
--               giving all terminal nodes the predictive distribution of t
--           store terminal's transition in transition map
--           - continue
--         ELSE
--           ENDFOR (break inner-most loop)
--         ENDIF
--       ENDFOR
--     ENDFOR
--   ENDFOR
--
--   // merge edgesets:
--   COLLECT map of (terminal nodes -> edgesets)
--     FILTER terminals in transition map keys where terminal.isEdgeSet == true
--     GROUPBY a terminal's distribution and the value from the transition map
--
--
--   FOR each terminal node belonging to an edgesets
--     SET terminal.edgeset to the corresponding set from the map
--         of (terminal nodes -> edgesets)
--
--   IF map of (terminal nodes -> edgesets) is nonEmpty
--   THEN set change = true
--   ENDIF
--
-- ENDUNTIL
--
-- Questions:
--   + if we let a terminal node's distribution override another terminal node's
--       distribution (via subtree) will order matter?
-------------------------------------------------------------------------------
refine :: Looping.Tree -> Looping.Tree
refine ltree' = go False ltree'
  where
    go :: Bool -> Looping.Tree -> Looping.Tree
    go stillDirty ltree = undefined

