module CSSR.Algorithm.Phase1Spec (spec) where

import CSSR.Prelude.Test
import CSSR.Fixtures (longEP)
import CSSR.Algorithm.Phase1 (initialization)
import Data.CSSR.Alphabet
import Data.Tree.Conditional
import qualified Data.Text as T
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified CSSR.Probabilistic as Prob


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  longEPSpec
  manual1101


longEPSpec :: Spec
longEPSpec =
  describe "a long even process" $ do
    let tree = initialization 3 longEP

    it "finds the correct alphabet" $
      mkAlphabet (HS.fromList ["0", "1"]) == alphabet tree

    let d0 = [root tree]
    describe "depth 0" $ depthSpec d0 [([1/3, 2/3],"")]

    let d1 = getChildren children d0
    describe "depth 1" $ do
      childrenSpec "0" d1 [([1/2, 1/2], "10")]
      childrenSpec "1" d1 [([0.0, 1.0], "01")]
      depthSpec d1
        [ ([1/2,1/2],"0")
        , ([1/4,3/4],"1")
        ]

    let d2 = getChildren children d1
    describe "depth 2" $ depthSpec d2
      [ ([1/2, 1/2],"00")
      , ([1/2, 1/2],"10")
      , ([0.0, 1.0],"01")
      , ([1/3, 2/3],"11")
      ]

    let d3 = getChildren children d2
    describe "depth 3" $ depthSpec d3
      [ ([1/2, 1/2],"000")
      , ([1/2, 1/2],"100")
      , ([1/2, 1/2],"110")
      , ([0.0, 1.0],"001")
      , ([0.0, 1.0],"101")
      , ([1/2, 1/2],"011")
      , ([1/4, 3/4],"111")
      ]
  where
    childrenSpec :: Text -> [Leaf] -> [([Double], Text)] -> SpecWith ()
    childrenSpec p ls exp = do
      it ("should have a node, "<> show p <> ", with at least " <> show (length exp) <> " children") $
        parent `shouldSatisfy` maybe False ((length exp <=) . HM.size . children)

      describe ("the children of" <> show p) $
        depthSpec childs exp

     where
      parent :: Maybe Leaf
      parent = find ((== txt2event p) . view lobsL) ls

      childs :: [Leaf]
      childs = maybe [] (HM.elems . view childrenL) parent

-- ========================================================================= --

-- | Hand-calculated initialization
--
-- 110111011101110111011101110111011101  i.e. (1101)+ as a regexp
--
-- Root (Pr(0)=1/4, Pr(1)=3/4)
--   0 (Pr(0)=0, Pr(1)=1)
--     00: never visited
--     10 (Pr(0)=0, Pr(1)=1)
--        010: never visited
--        110: (Pr(0)=0, Pr(1)=1)
--          0110: never visited
--          1110 (Pr(0)=0, Pr(1)=1)
--            01110 (Pr(0)=0, Pr(1)=1)
--            11110: never visited
--   1 (Pr(0)=1/3, Pr(1)=2/3)
--     01 (Pr(0)=0, Pr(1)=1)
--       001: never visited
--       101 (Pr(0)=0, Pr(1)=1)
--         0101: never visited
--         1101 (Pr(0)=0, Pr(1)=1)
--     11 (Pr(0)=1/2, Pr(1)=1/2)
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
manual1101 :: Spec
manual1101 =
  describe "a (1101)+ regex" $ do
    let tree = initialization 6 $ T.unpack $ T.replicate 100 "1101"

    it "finds the correct alphabet" $
      mkAlphabet (HS.fromList ["0", "1"]) == alphabet tree

    let d0 = [root tree]
    describe "depth 0" $ depthSpec d0
      [ ([1/4, 3/4], "")
      ]

    let d1 = getChildren children d0
    describe "depth 1" $ depthSpec d1
      [ ([  0,  1],"0")
      , ([1/3,2/3],"1")
      ]

    let d2 = getChildren children d1
    describe "depth 2" $ depthSpec d2
      [ ([  0,   1],"10")
      , ([  0,   1],"01")
      , ([1/2, 1/2],"11")
      ]

    let d3 = getChildren children d2
    describe "depth 3" $ depthSpec d3
      [ ([0, 1], "110")
      , ([0, 1], "101")
      , ([0, 1], "011")
      , ([1, 0], "111")
      ]

depthSpec :: [Leaf] -> [([Double],Event)] -> Spec
depthSpec cs exps =
  forM_ exps $ \(dist, txtObs) ->
    it ("should have an observation of " <> T.unpack txtObs <> " with distribution: " <> show dist) $
      find ((== txt2event txtObs) . snd) check `shouldSatisfy` maybe False (isApprox 0.05 (V.fromList dist) . fst)
 where
  check :: [(Vector Double, Vector Event)]
  check = toCheckable cs

  toCheckable :: [Leaf] -> [(Vector Double, Vector Event)]
  toCheckable = map ((Prob.freqToDist . view lfrequencyL) &&& view lobsL)


