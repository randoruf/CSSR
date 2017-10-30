module CSSR.Algorithm.Phase2Spec where

import CSSR.Prelude.Test
import CSSR.Prelude.Mutable (runST)

import Control.Applicative ((<|>))
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
  describe "a long even process root" $ do
    let ltree = runST $ grow 0.01 (initialization 3 longEP) >>= ML.freezeTree

    describe "the root looping node" $ do
      it "should have the expected frequency" $
        Prob.freqToDist <$> preview (L.rootL . L.bodyL . _Right . L.frequencyL) ltree `shouldSatisfy`
          maybe False (isApprox 0.05 (V.fromList [1/3, 2/3]))

      context "its histories" $
        historiesMatchSpec "root" (L.root ltree) [([1/3, 2/3], "")]

    let d1 = maybe [] HM.elems $ preview (L.rootL . L.childrenL) ltree :: [L.Leaf]
    describe "depth 1" $ depthSpec d1
      [ Right ([1/2, 1/2], "0", [])
      , Right ([1/4, 3/4], "1", ["1", "0"])
      ]

    let d2 = foldMap (HM.elems . L.children) d1
    describe "depth 2" $ depthSpec d2
      [ Left ""
      , Right ([0, 1], "01", [])
      ]

    let d3 = foldMap (HM.elems . L.children) d2
    describe "depth 3" $
      it "should be empty" $ null d3

  where
    toCheckableHists :: L.LeafBody -> HashSet (Vector Double, Vector Event)
    toCheckableHists = HS.map ((Prob.freqToDist . view Cond.lfrequencyL) &&& view Cond.lobsL) . L.histories

    historiesMatchSpec :: Text -> L.Leaf -> [([Double], Text)] -> Spec
    historiesMatchSpec t ll = mapM_ $ \(dist, txtObs) ->
      it ("should have a history of " <> T.unpack txtObs <> " with distribution: " <> show dist) $
        (tocheck >>= find ((== txt2event txtObs) . snd)) `shouldSatisfy`
            maybe False (isApprox 0.05 (V.fromList dist) . fst)
      where
        tocheck :: Maybe [(Vector Double, Vector Event)]
        tocheck = toCheckableHists <$> preview (L.bodyL . _Right . L.historiesL) ll

        toCheckableHists :: HashSet Cond.Leaf -> [(Vector Double, Vector Event)]
        toCheckableHists = HS.toList . HS.map ((Prob.freqToDist . view Cond.lfrequencyL) &&& view Cond.lobsL)

    depthSpec :: [L.Leaf] -> [Either Text ([Double], Text, [Event])] -> Spec
    depthSpec ls = mapM_ tester
     where
      tester :: Either Text ([Double], Text, [Event]) -> Spec
      tester (Left loop) =
        it ("should have a loop to " <> show loop) $
          foldr (finder loop . tocheckable) False ls `shouldBe` True
       where
        finder
          :: Text -> Either (Vector Event) (Vector Double, [Vector Event], [Event]) -> Bool -> Bool
        finder tobs (Left x)  memo = memo || txt2event tobs == x
        finder _    (Right _) memo = memo


      tester (Right (dist, tobs, nxts)) =
        it ("should have a leaf " <> show tobs <> " with distribution: " <> show dist
            <> " and children at " <> show nxts) $
          foldr (finder tobs . tocheckable) Nothing ls `shouldSatisfy`
                maybe False (predicate dist nxts)
       where
        finder
          :: Text -> Either (Vector Event) (Vector Double, [Vector Event], [Event])
          -> Maybe (Vector Double, [Vector Event], [Event])
          -> Maybe (Vector Double, [Vector Event], [Event])
        finder _    (Left _)  memo = memo
        finder tobs (Right x) memo = memo
          <|> maybe Nothing (const $ Just x) (find (== txt2event tobs) (view _2 x))


      tocheckable :: L.Leaf -> Either (Vector Event) (Vector Double, [Vector Event], [Event])
      tocheckable l = case L.body l of
        Left (L.LeafRep os) -> Left os
        Right bod -> Right (bod2dist bod, bod2obs bod, HM.keys (L.children l))
       where
        bod2dist :: L.LeafBody -> Vector Double
        bod2dist =Prob.freqToDist . L.frequency

        bod2obs :: L.LeafBody -> [Vector Event]
        bod2obs = map (view Cond.lobsL) . HS.toList . L.histories

      predicate :: [Double] -> [Event] -> (Vector Double, [Vector Event], [Event]) -> Bool
      predicate d0 cs0 (d1, _, cs1) = isApprox 0.05 (V.fromList d0) d1 && HS.fromList cs0 == HS.fromList cs1


