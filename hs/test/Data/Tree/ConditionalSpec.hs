module Data.Tree.ConditionalSpec where

import CSSR.Prelude.Test
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.HashSet as HS

import CSSR.Fixtures (shortEP)
import Data.Alphabet (mkAlphabet)

import qualified Data.Tree.Conditional as Cond
import qualified Data.MTree.Parse      as Parse (buildTree)
import qualified Data.Tree.ParseSpec   as Parse (tree) -- use the tree from the parse spec for testing

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "converting a parse tree to a conditional tree" $ do
    let tree = Cond.convert Parse.tree
    it "removes the last children from a Parse Tree" $
      all (isNothing . Cond.navigate tree) $ fmap txt2event ["abc", "bcc"]

    it "keeps the children of last depth" $
      all (isJust . Cond.navigate tree) $ fmap txt2event ["bc", "cc"]

  describe "a short even process" $ do
    let s = V.fromList . fmap T.singleton $ shortEP
    let ptree = Parse.buildTree 3 s
    let tree = Cond.convert ptree

    it "finds the correct alphabet" $
      mkAlphabet (HS.fromList ["0", "1"]) == Cond.alphabet tree

    let d0 = [Cond.root tree]
    describe "depth 0" $ depthSpec d0 [([0.33,0.67],"")]

    let d1 = getChildren' d0
    describe "depth 1" $ do
      childrenSpec "0" d1 [([0.5, 0.5], "10")]
      childrenSpec "1" d1 [([  0, 1.0], "01")]
      depthSpec d1
        [ ([ 0.5, 0.5],"0")
        , ([0.25,0.75],"1")
        ]

    let d2 = getChildren' d1
    describe "depth 2" $
      depthSpec d2
        [ ([ 0.5, 0.5],"00")
        , ([ 0.5, 0.5],"10")
        , ([   0,   1],"01")
        , ([0.33,0.66],"11")
        ]

    let d3 = getChildren' d2
    describe "depth 3" $ depthSpec d3

      [ ([ 0.5, 0.5],"000")
      , ([ 0.5, 0.5],"100")
      , ([ 0.5, 0.5],"110")
      , ([   0,   1],"001")
      , ([   0,   1],"101")
      , ([ 0.5, 0.5],"011")
      , ([0.33,0.67],"111")
      ]

  where
    bodyShouldContain :: [Cond.Leaf] -> [([Double], Event)] -> Expectation
    bodyShouldContain
      = nodesShouldContain epsilon (Cond.frequency . Cond.body) (Cond.obs . Cond.body)

    getChildren' :: [Cond.Leaf] -> [Cond.Leaf]
    getChildren' = getChildren Cond.children

    childrenSpec :: Text -> [Cond.Leaf] -> [([Double], Text)] -> SpecWith ()
    childrenSpec p d exp =
      it ("should have a node, "<> show p <>", with children " <> show exp) $ do
        let childs = fmap (getChildren_ Cond.children) (findObs (Cond.obs . Cond.body) p d)
        guard (isJust childs)
        bodyShouldContain (fromJust childs) exp

    depthSpec :: [Cond.Leaf] -> [([Double],Event)] -> SpecWith (Arg Expectation)
    depthSpec d exp = do
      it "should contain expected number of leaves" $ length d `shouldBe` length exp
      it "should contain expected leaves"           $ d `bodyShouldContain` exp

    epsilon :: Double
    epsilon = 0.1 -- high epsilon since we are working with a very small sample size (60ish characters)

