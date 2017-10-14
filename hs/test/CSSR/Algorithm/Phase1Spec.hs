module CSSR.Algorithm.Phase1Spec where

import CSSR.Prelude.Test
import CSSR.Fixtures (short_ep)
import CSSR.Algorithm.Phase1 (initialization)
import Data.Alphabet
import Data.Tree.Hist
import qualified Data.Text as T
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "a short even process" $ do
    let tree = initialization 2 short_ep

    it "finds the correct alphabet" $
      mkAlphabet (HS.fromList ["0", "1"]) == alphabet tree

    let d0 = [root tree]
    describe "depth 0" $ depthSpec d0 [([28,42],"")]

    let d1 = getChildren d0
    describe "depth 1" $ depthSpec d1 [([15,12],"0"), ([12,30],"1")]

    let d2 = getChildren d1
    describe "depth 2" $ depthSpec d2 [([1,1],"00"), ([0,1], "10"), ([1,1],"01"), ([1,1],"11")]


  where
    str2Event :: Event -> [Event]
    str2Event = fmap T.singleton . T.unpack

    bodyShouldContain :: [Leaf] -> [([Integer],Event)] -> Expectation
    bodyShouldContain d cs = fmap ((frequency . body) &&& (obs . body)) d
             `shouldContain` fmap ( V.fromList        *** (V.fromList . str2Event)) cs

    getChildren :: [Leaf] -> [Leaf]
    getChildren = foldr ((<>) . HM.elems . children) []

    depthSpec :: [Leaf] -> [([Integer],Event)] -> SpecWith (Arg Expectation)
    depthSpec d exp = do
      it "should contain expected number of leaves" $ length d `shouldBe` length exp
      it "should contain expected leaves"           $ d `bodyShouldContain` exp




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
