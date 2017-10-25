module CSSR.Algorithm.Phase2Spec where

import CSSR.Prelude.Test
import CSSR.Prelude.Mutable (runST)

import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

import CSSR.Fixtures (longEP)
import CSSR.Algorithm.Phase1 (initialization)
import CSSR.Algorithm.Phase2 (grow)
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
  describe "a short even process root" $ do
    let ltree = runST $ grow 0.01 (initialization 1 longEP) >>= ML.freezeTree

    describe "the root looping node" $ do
      it "should have the expected frequency" $
        Prob.freqToDist <$> preview (L.rootL . L.bodyL . _Right . L.frequencyL) ltree `shouldSatisfy`
          maybe False (isApprox 0.05 (V.fromList [0.33,0.66]))

      context "its histories" $
        historiesMatchSpec "root" (L.root ltree) [([0.33,0.66], "")]

  where
    toCheckableHists :: L.LeafBody -> HashSet (Vector Double, Vector Event)
    toCheckableHists = HS.map ((Prob.freqToDist . view Cond.lfrequencyL) &&& view Cond.lobsL) . L.histories

    historiesMatchSpec :: Text -> L.Leaf -> [([Double], Text)] -> Spec
    historiesMatchSpec t ll = mapM_ $ \(dist, txtObs) ->
      it ("should have a history of " <> T.unpack txtObs <> " with distribution: " <> show dist) $
        (check >>= find ((== txt2event txtObs) . snd)) `shouldSatisfy`
            maybe False (isApprox 0.05 (V.fromList dist) . fst)
      where
        check :: Maybe [(Vector Double, Vector Event)]
        check = toCheckableHists <$> preview (L.bodyL . _Right . L.historiesL) ll

        toCheckableHists :: HashSet Cond.Leaf -> [(Vector Double, Vector Event)]
        toCheckableHists = HS.toList . HS.map ((Prob.freqToDist . view Cond.lfrequencyL) &&& view Cond.lobsL)


