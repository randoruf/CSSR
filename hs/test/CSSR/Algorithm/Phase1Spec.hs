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
    let tree = initialization 3 short_ep

    it "finds the correct alphabet" $
      mkAlphabet (HS.fromList ["0", "1"]) == alphabet tree

    let d0 = [root tree]
    describe "depth 0" $ depthSpec d0 [([27,42],"")]

    let d1 = getChildren d0
    describe "depth 1" $ do
      childrenSpec "0" d1 [([6, 8], "10")]
      childrenSpec "1" d1 [([0,12], "01")]
      depthSpec d1
        [ ([15,12],"0")
        , ([12,30],"1")
        ]

    let d2 = getChildren d1
    describe "depth 2" $
      depthSpec d2
        [ ([ 6, 8],"00")
        , ([ 8, 4],"10")
        , ([ 0,12],"01")
        , ([12,18],"11")
        ]

    let d3 = getChildren d2
    describe "depth 3" $ depthSpec d3
      [ ([1,1],"000")
      , ([1,1],"100")
      , ([1,1],"110")
      , ([0,1],"001")
      , ([0,1],"101")
      , ([1,1],"011")
      , ([1,1],"111")
      ]
  where
    str2Event :: Event -> Vector Event
    str2Event = V.fromList . fmap T.singleton . T.unpack

    bodyShouldContain :: [Leaf] -> [([Integer],Event)] -> Expectation
    bodyShouldContain d cs = fmap ((frequency . body) &&& (obs . body)) d
             `shouldContain` fmap ( V.fromList        ***  str2Event  ) cs

    findObs :: Text -> [Leaf] -> Maybe Leaf
    findObs es = find ((== str2Event es) . view (bodyL.obsL))

    getChildren :: [Leaf] -> [Leaf]
    getChildren = foldr ((<>) . HM.elems . children) []

    getChildren_ :: Leaf -> [Leaf]
    getChildren_ = HM.elems . children

    childrenSpec :: Text -> [Leaf] -> [([Integer], Text)] -> SpecWith ()
    childrenSpec p d exp =
      it ("should have a node, "<> show p <>", with children " <> show exp) $ do
        let childs = fmap getChildren_ (findObs p d)
        guard (isJust childs)
        bodyShouldContain (fromJust childs) exp

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
