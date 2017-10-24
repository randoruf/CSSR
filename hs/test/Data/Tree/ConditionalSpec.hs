module Data.Tree.ConditionalSpec where

import CSSR.Prelude.Test
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM

import CSSR.Fixtures (short_ep)
import Data.Alphabet (mkAlphabet)

import qualified CSSR.Probabilistic    as Prob
import qualified Data.Tree.Conditional as Cond
import qualified Data.MTree.Parse      as Parse (getAlphabet, buildTree)
import qualified Data.Tree.Parse       as Parse
import qualified Data.Tree.ParseSpec   as Parse (tree) -- use the tree from the parse spec for testing

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "converting a parse tree to a conditional tree" $ do
    let tree = Cond.convert Parse.tree (Parse.getAlphabet Parse.tree)
    it "removes the last children from a Parse Tree" $
      all (isNothing . Cond.navigate tree) $ fmap txt2event ["abc", "bcc"]

    it "keeps the children of last depth" $
      all (isJust . Cond.navigate tree) $ fmap txt2event ["ab", "bc"]

  describe "a short even process" $ do
    let s = V.fromList . fmap T.singleton $ short_ep
    let ptree = Parse.buildTree 3 s
    let tree = Cond.convert ptree (Parse.getAlphabet ptree)

    it "finds the correct alphabet" $
      mkAlphabet (HS.fromList ["0", "1"]) == Cond.alphabet tree

    let d0 = [Cond.root tree]
    describe "depth 0" $ depthSpec d0 [([0.33,0.67],"")]

    let d1 = getChildren d0
    describe "depth 1" $ do
      childrenSpec "0" d1 [([0.5, 0.5], "10")]
      childrenSpec "1" d1 [([  0, 1.0], "01")]
      depthSpec d1
        [ ([ 0.5, 0.5],"0")
        , ([0.25,0.75],"1")
        ]

    let d2 = getChildren d1
    describe "depth 2" $
      depthSpec d2
        [ ([ 0.5, 0.5],"00")
        , ([ 0.5, 0.5],"10")
        , ([   0,   1],"01")
        , ([0.33,0.66],"11")
        ]

    let d3 = getChildren d2
    describe "depth 3" $ depthSpec d3

      [ ([ 0.5, 0.5],"000")
      , ([ 0.5, 0.5],"100")
      , ([ 0.5, 0.5],"110")
      , ([   0,   1],"001")
      , ([   0,   1],"101")
      , ([0.33,0.67],"011")
      , ([0.33,0.67],"111")
      ]

  -- 110111011101110111011101110111011101  i.e. (1101)+ as a regexp
  xdescribe "a (1101)+ regex" $ do
    let s = V.fromList . fmap T.singleton . T.unpack $ T.replicate 100 "1101"
    let ptree = Parse.buildTree 6 s
    let tree = Cond.convert ptree (Parse.getAlphabet ptree)

    it "finds the correct alphabet" $
      mkAlphabet (HS.fromList ["0", "1"]) == Cond.alphabet tree

    let d0 = [Cond.root tree]
    describe "depth 0" $ depthSpec d0 [([99,296],"")]

    let d1 = getChildren d0
    describe "depth 1" $ do
      depthSpec d1
        [ ([  0, 99],"0")
        , ([100,196],"1")
        ]

    let d2 = getChildren d1
    describe "depth 2" $
      depthSpec d2
        -- 0 children
        [ ([  0, 99],"10")
        -- 1 children
        , ([  0,100],"01")
        , ([ 98, 98],"11")
        ]

    let d3 = getChildren d2
    describe "depth 3" $ depthSpec d3
      --10 (Pr(0)=0, Pr(1)=1)
      [ ([0, 99], "110")
      --01
      , ([0,100], "101")
      --11
      , ([0, 98], "011")
      , ([98, 0], "111")
      ]


--   --10 (Pr(0)=0, Pr(1)=1)
--        010: never visited
--        110: (Pr(0)=0, Pr(1)=1)
--          0110: never visited
--          1110 (Pr(0)=0, Pr(1)=1)
--            01110 (Pr(0)=0, Pr(1)=1)
--            11110: never visited
--   --01 (Pr(0)=0, Pr(1)=1)
--       001: never visited
--       101 (Pr(0)=0, Pr(1)=1)
--         0101: never visited
--         1101 (Pr(0)=0, Pr(1)=1)
--   --11 (Pr(0)=1/2, Pr(1)=1/2)
--       011 (Pr(0)=0, Pr(1)=1)
--         0011: never visited
--         1011 (Pr(0)=0, Pr(1)=1)
--           01011: never visited
--           11011 (Pr(0)=0, Pr(1)=1)
--       111 (Pr(0)=1, Pr(1)=0)
--         0111 (Pr(0)=1, Pr(1)=0)
--           00111: never visited
--           10111 (Pr(0)=1, Pr(1)=0)
--         1111: never visited



  where
    txt2event :: Event -> Vector Event
    txt2event = V.fromList . fmap T.singleton . T.unpack

    leaf2dist :: Cond.Leaf -> Vector Double
    leaf2dist = Prob.freqToDist . Cond.frequency . Cond.body

    bodyShouldContain :: [Cond.Leaf] -> [([Double], Event)] -> Expectation
    bodyShouldContain d cs =
      forM_ cs $ \(dist, e) -> do
        let found = find ((txt2event e ==) . Cond.obs . Cond.body) d
        found `shouldSatisfy` maybe False ((V.fromList dist ~=) . leaf2dist)

    findObs :: Text -> [Cond.Leaf] -> Maybe Cond.Leaf
    findObs es = find ((== txt2event es) . view (Cond.bodyL . Cond.obsL))

    getChildren :: [Cond.Leaf] -> [Cond.Leaf]
    getChildren = foldr ((<>) . getChildren_) []

    getChildren_ :: Cond.Leaf -> [Cond.Leaf]
    getChildren_ = HM.elems . Cond.children

    childrenSpec :: Text -> [Cond.Leaf] -> [([Double], Text)] -> SpecWith ()
    childrenSpec p d exp =
      it ("should have a node, "<> show p <>", with children " <> show exp) $ do
        let childs = fmap getChildren_ (findObs p d)
        guard (isJust childs)
        bodyShouldContain (fromJust childs) exp

    depthSpec :: [Cond.Leaf] -> [([Double],Event)] -> SpecWith (Arg Expectation)
    depthSpec d exp = do
      it "should contain expected number of leaves" $ length d `shouldBe` length exp
      it "should contain expected leaves"           $ d `bodyShouldContain` exp

    isApprox :: (Ord f, Fractional f) => f -> Vector f -> Vector f -> Bool
    isApprox eps d0 d1
      = length d0 == length d1
      && all ((< eps) . abs) (V.zipWith (-) d0 d1)

    (~=) :: (Ord f, Fractional f) => Vector f -> Vector f -> Bool
    (~=) = isApprox 0.1 -- high epsilon since we are working with a very small sample size (60ish characters)

