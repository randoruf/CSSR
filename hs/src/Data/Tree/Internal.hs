{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Data.Tree.Internal where

import Data.Functor.Identity (Identity(..))
import CSSR.Prelude
import CSSR.Probabilistic (Probabilistic, TestResult(..))
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
isHomogeneous getChildren child2dist sig parent =
  runIdentity $ isHomogeneousM (pure . getChildren) (pure . child2dist) sig parent


isHomogeneousM
  :: Monad m
  => (parent -> m [child])
  -> (child -> m (Vector Integer))
  -> Double
  -> (parent, Vector Integer)
  -> m Bool
isHomogeneousM getChildrenM child2distM sig (parent, pdist) =
  getChildrenM parent
  >>= mapM child2distM
  >>= pure . all ((== Significant) . (Prob.matchesFreqs sig pdist))



navigate :: forall lf . (lf -> Event -> Maybe lf) -> lf -> Vector Event -> Maybe lf
navigate lookup rt history = runIdentity $ navigateM (\a b -> pure $ lookup a b) rt history


navigateM :: forall lf m . Monad m => (lf -> Event -> m (Maybe lf)) -> lf -> Vector Event -> m (Maybe lf)
navigateM lookup rt history
  | V.null history = pure (Just rt)
  | otherwise      = go (fromIntegral (V.length history)) rt
  where
    go :: Natural -> lf -> m (Maybe lf)
    go 0 lf = pure (Just lf)
    go d lf =
      lookup lf (history ! nxt)
      >>= \case
        Just ch -> go (d - 1) ch
        Nothing -> pure Nothing
      where
        nxt :: Int
        nxt = fromIntegral (d - 1)


-- | returns ancestors in order of how they should be processed
getAncestors :: (l -> Maybe l) -> l -> [l]
getAncestors getParent l = runIdentity $ getAncestorsM (pure . getParent) l

getAncestorsM :: forall l m . Monad m => (l -> m (Maybe l)) -> l -> m [l]
getAncestorsM getParent l = go (Just l) []
  where
    go :: Maybe l -> [l] -> m [l]
    go  Nothing ancestors = pure ancestors
    go (Just w) ancestors = getParent w >>= \p -> go p (w:ancestors)




