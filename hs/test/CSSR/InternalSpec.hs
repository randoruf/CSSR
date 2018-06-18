module CSSR.InternalSpec where

import CSSR.Prelude.Test
import CSSR.Prelude.Mutable

import Numeric.CSSR.Algorithm.Phase1 (initialization)
import Numeric.CSSR.Algorithm.Phase2 (grow)
import Numeric.CSSR.Algorithm.Phase3 (refine)

import qualified Data.Tree.Conditional as Cond
import qualified Data.Tree.Looping as L
import qualified Data.MTree.Looping as ML
import qualified Data.Vector as V

import CSSR.Fixtures
import CSSR.Internal (pathToState)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  -- describe "statesAndTransitions" $ statesAndTransitionsSpec
  describe "pathToState" $ pathToStateSpec

statesAndTransitionsSpec :: Spec
statesAndTransitionsSpec = do
  undefined


pathToStateSpec :: Spec
pathToStateSpec = do
  describe "a long even process root" $ do
    let
      terms = L.terminals ltree
      term0 = find (\n -> L.path n == V.fromList ["0"]) terms
      term1 = find (\n -> L.path n == V.fromList ["0","1"]) terms

    it "should have the expected terminals" $ do
      term0 `shouldSatisfy` isJust
      term1 `shouldSatisfy` isJust
      length terms `shouldBe` 2
    describe "the '0' terminal" $ do
      let t0 = fromJust term0
      it "should have a pathToState from '0' to itself at '00'" $
        pathToState ltree t0 ("0", 0) `shouldBe` term0

      it "should have a pathToState from '1' to t1 at '01'" $
        pathToState ltree t0 ("1", 1) `shouldBe` term1

    describe "the '01' terminal" $ do
      let t0 = fromJust term0
      it "should have a pathToState from '0' to t0 at '010'" $
        pathToState ltree t0 ("0", 0) `shouldBe` term0

      it "should have a pathToState from '1' to t0, via looping, at '011'" $
        pathToState ltree t0 ("1", 1) `shouldBe` term1


 where
  ltree :: L.Tree
  ltree = runST $ do
    let ptree = initialization 3 longEP
    mltree <- grow 0.01 ptree
    refine (Cond.alphabet ptree) mltree
    ML.freezeTree mltree

