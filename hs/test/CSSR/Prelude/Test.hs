module CSSR.Prelude.Test
  ( module X
  , txt2event
  , nodesShouldContain
  , findObs
  , getChildren
  , getChildren_
  , isApprox
  ) where

import CSSR.Prelude as X

import Data.Maybe as X
import Test.Hspec as X
import Test.Hspec.QuickCheck as X

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM (elems)
import qualified CSSR.Probabilistic as Prob (freqToDist)


txt2event :: Text -> Vector Event
txt2event = V.fromList . fmap T.singleton . T.unpack

nodesShouldContain :: Show l => Double -> (l -> Vector Integer) -> (l -> Vector Event) -> [l] -> [([Double], Event)] -> Expectation
nodesShouldContain e toF toO ls exs =
  forM_ exs $ \(dist, ex) -> do
    let found = find ((txt2event ex ==) . toO) ls
    findObs toO ex ls `shouldSatisfy` maybe False (isApprox e (V.fromList dist) . Prob.freqToDist . toF)

findObs :: (l -> Vector Event) -> Text -> [l] -> Maybe l
findObs toO exp = find ((== txt2event exp) . toO)

getChildren :: (l -> HashMap Event l) -> [l] -> [l]
getChildren f = foldr ((<>) . getChildren_ f) []

getChildren_ :: (l -> HashMap Event l) -> l -> [l]
getChildren_ toChs = HM.elems . toChs

isApprox :: (Ord f, Fractional f) => f -> Vector f -> Vector f -> Bool
isApprox eps d0 d1
  = length d0 == length d1
  && all ((< eps) . abs) (V.zipWith (-) d0 d1)

