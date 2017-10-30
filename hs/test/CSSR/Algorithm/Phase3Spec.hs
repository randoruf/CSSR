module CSSR.Algorithm.Phase3Spec where

import CSSR.Prelude.Test
import CSSR.Prelude.Mutable (runST)

import qualified Data.HashSet as HS
import qualified Data.Vector as V

import CSSR.Fixtures (longEP)
import CSSR.Algorithm.Phase1 (initialization)
import CSSR.Algorithm.Phase2 (grow)
import CSSR.Algorithm.Phase3 (refine)
import Data.Alphabet

import qualified CSSR.Probabilistic as Prob
import qualified Data.Tree.Conditional as Cond
import qualified Data.MTree.Looping as ML
import qualified Data.Tree.Looping as L
import qualified Data.Text as T


main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "a long even process root" $ do
    let
      ltree0 = runST $ do
        let ptree = initialization 3 longEP
        mltree <- grow 0.01 ptree
        ML.freezeTree mltree

      ltree1 = runST $ do
        let ptree = initialization 3 longEP
        mltree <- grow 0.01 ptree
        refine (Cond.alphabet ptree) mltree
        ML.freezeTree mltree

    it "should be identical to the grown looping tree in Phase 2" $
      ltree0 == ltree1

    it "should have the same terminal nodes as the grown looping tree" $
      L.terminals ltree0 == L.terminals ltree1

