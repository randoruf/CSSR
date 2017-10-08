{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Data.Tree.Internal where

import CSSR.Prelude
import CSSR.Probabilistic (Probabilistic)
import qualified CSSR.Probabilistic as Prob
import qualified Data.Vector as V



-- | === Homogeneity
-- Psuedocode from paper:
--   INPUTS: looping node, parse tree
--   COLLECT all next-step histories from looping node in parse tree
--   FOR each history in next-step histories
--     FOR each child in history's children
--       IF child's distribution ~/=  node's distribution
--       THEN RETURN false
--       ENDIF
--     ENDFOR
--   ENDFOR
--   RETURN TRUE
--
isHomogeneous
  :: (parent -> [child])
  -> (child -> Vector Integer)
  -> Double
  -> (parent, Vector Integer)
  -> Bool
isHomogeneous getChildren child2dist sig (parent, pdist) =
  all (cMatchesP . child2dist)
  . getChildren
  $ parent

 where
  cMatchesP :: Vector Integer -> Bool
  cMatchesP cdist = Prob.matchesDists_ pdist cdist sig


navigate :: forall lf . (lf -> Event -> Maybe lf) -> lf -> Vector Event -> Maybe lf
navigate lookup rt history
  | V.null history = Just rt
  | otherwise = go (V.length history) rt
  where
    go :: Int -> lf -> Maybe lf
    go 0 lf = Just lf
    go d lf =
      let nxt = d - 1
      in case lookup lf (history ! nxt) of
        Just child -> go nxt child
        _ -> Nothing


-- | returns ancestors in order of how they should be processed
getAncestors :: forall l . (l -> Maybe l) -> l -> [l]
getAncestors getParent l = go (Just l) []
  where
    go :: Maybe l -> [l] -> [l]
    go  Nothing ancestors = ancestors
    go (Just w) ancestors = go (getParent w) (w:ancestors)



