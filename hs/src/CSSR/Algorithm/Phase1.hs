-------------------------------------------------------------------------------
-- |
-- == Phase I: "Initialization"
--
-- Requires estimates of conditional probabilities to converge, perhaps rapidly.
--
-------------------------------------------------------------------------------
module CSSR.Algorithm.Phase1 where

import CSSR.Prelude
import qualified Data.Text    as T
import qualified Data.Vector  as V

import qualified Data.Tree.Looping  as Looping (Tree)
import qualified Data.MTree.Parse   as M
import qualified Data.Tree.Parse    as P
import qualified Data.Tree.Hist     as Hist
import qualified Data.MTree.Parse   as MHist


-------------------------------------------------------------------------------
-- |
-- FIXME: Currently only works with characters
--
-- Examples:
--
-- >>> let short_ep = "00011110001100011110000111101101111111111000110001101101100111100111100"
-- >>> initialization 1 short_ep
-- Tree {depth 1, Alphabet: ["0","1"]}
--   root:
--      " "->Leaf{obs: [], freq: [28,42]}
--           children:
--           "0"->Leaf{obs: ["0"], freq: [1,1], no children}
-- <BLANKLINE>
--           "1"->Leaf{obs: ["1"], freq: [1,1], no children}
-- >>> initialization 2 short_ep
-- Tree {depth 2, Alphabet: ["0","1"]}
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
-------------------------------------------------------------------------------
initialization :: Int -> [Char] -> Hist.Tree
initialization depth (fmap T.singleton -> s) =
  Hist.convert parseTree $ M.getAlphabet parseTree
  where
    parseTree :: P.Tree
    parseTree = M.buildTree depth . V.fromList $ s


