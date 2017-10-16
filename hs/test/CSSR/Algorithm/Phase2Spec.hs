module CSSR.Algorithm.Phase2Spec where

import CSSR.Prelude.Test
import CSSR.Prelude.Mutable (runST)

import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

import CSSR.Fixtures (short_ep)
import CSSR.Algorithm.Phase1 (initialization)
import CSSR.Algorithm.Phase2 (grow)
import Data.Alphabet

import qualified Data.Tree.Hist as Hist
import qualified Data.MTree.Looping as ML
import qualified Data.Tree.Looping as L
import qualified Data.Text as T


main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "a short even process root" $ do
    let ltree = runST $ grow 0.01 (initialization 1 short_ep) >>= ML.freezeTree
    it "should have the expected frequency" $
      L.frequency <$> (rootBody ltree) `shouldBe` Just (V.fromList [28,42])
    it "should have the expected histories" $
      (histories . L.histories) <$> (rootBody ltree) `shouldBe` Just (asExps [([28,42], "")])
  where
    rootBody :: L.Tree -> Maybe L.LeafBody
    rootBody = preview (L.rootL . L.bodyL . _Right)

    histories :: HashSet Hist.Leaf -> HashSet (Vector Integer, Vector Event)
    histories = HS.map ((view Hist.lfrequencyL) &&& (view Hist.lobsL))

    asExps :: [([Integer], Text)] -> HashSet (Vector Integer, Vector Event)
    asExps es = HS.fromList $ fmap asExp es
      where
        asExp :: ([Integer], Text) -> (Vector Integer, Vector Event)
        asExp (is, es) = (V.fromList is, V.fromList . map T.singleton . T.unpack $ es)


