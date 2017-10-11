module CSSR.Algorithm.Phase1Spec where

import CSSR.Prelude.Test
import CSSR.Algorithm.Phase1 (initialization)
import Data.Alphabet
import Data.Tree.Hist
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "a short even process" $ do
    let short_ep = "00011110001100011110000111101101111111111000110001101101100111100111100"
    let tree = initialization 2 short_ep
    it "finds the correct alphabet" $
      mkAlphabet (HS.fromList ["0", "1"]) == alphabet tree
    let d0 = root tree :: Leaf
    let d1 = HM.elems (children d0) :: [Leaf]
    let d2 = foldr ((<>) . HM.elems . children) [] d1 :: [Leaf]
    it "finds the correct children of depth 2" $
      HS.fromList (fmap V.fromList [["0", "0"],["1", "0"],["0", "1"], ["1", "1"]]) == HS.fromList (fmap (obs . body) d2)

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
-- initialization :: Int -> [Char] -> Hist.Tree
-- initialization depth (fmap T.singleton -> s) =
--   Hist.convert parseTree $ M.getAlphabet parseTree
--   where
--     parseTree :: P.Tree
--     parseTree = M.buildTree depth . V.fromList $ s
