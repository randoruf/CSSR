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
  :: (parent -> [Vector Integer])
  -> Double
  -> (parent, Vector Integer)
  -> Bool
isHomogeneous getChildDists sig parent =
  runIdentity $ isHomogeneousM (pure . getChildDists) sig parent

-- isHomogeneous :: forall s . Double -> MLeaf s -> ST s Bool
-- isHomogeneous sig ll = do
--   pdist <- readSTRef $ frequency ll
--   childs <- childHistories ll
--   hists <- l2hists ll
--   pure $ all (cMatchesP pdist . Prob.frequency) childs
--   where
--     l2hists :: MLeaf s -> ST s [Vector Integer]
--     l2hists lf = do
--       hs <- fmap toList . readSTRef . histories $ lf
--       pure $ fmap (view (Hist.bodyL . Hist.frequencyL)) hs
--
--     cMatchesP :: Vector Integer -> Vector Integer -> Bool
--     cMatchesP pdist cdist = Prob.matchesFreqs sig pdist cdist == Significant

isHomogeneousM
  :: Monad m
  => (parent -> m [Vector Integer])
  -> Double
  -> (parent, Vector Integer)
  -> m Bool
isHomogeneousM getChildDists sig (parent, pdist) =
  all ((== Significant) . (Prob.matchesFreqs sig pdist))
  <$> getChildDists parent


navigate :: forall lf . (lf -> Event -> Maybe lf) -> lf -> Vector Event -> Maybe lf
navigate lookup rt history =
  runIdentity $ navigateM (\a b -> pure $ lookup a b) rt history


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
getAncestors getParent l =
  runIdentity $ getAncestorsM (pure . getParent) l

getAncestorsM :: forall l m . Monad m => (l -> m (Maybe l)) -> l -> m [l]
getAncestorsM getParent l = getParent l >>= go []
  where
    go :: [l] -> Maybe l -> m [l]
    go ancestors  Nothing = pure ancestors
    go ancestors (Just w) = getParent w >>= go (w:ancestors)



-- | === Excisability
-- Psuedocode from paper:
--   INPUTS: looping node, looping tree
--   COLLECT all ancestors of the looping node from the looping tree, ordered by
--           increasing depth (depth 0, or "root node," first)
--   FOR each ancestor
--     IF ancestor's distribution == looping node's distribution
--     THEN
--       the node is excisable: create loop in the tree
--       ENDFOR (ie "break")
--     ELSE do nothing
--     ENDIF
--   ENDFOR
excisable :: (l -> Maybe l) -> (l -> Vector Integer) -> Double -> l -> Maybe l
excisable getParent getFrequency sig l =
  runIdentity $ excisableM (pure . getParent) (pure . getFrequency) sig l

excisableM :: forall l m . Monad m => (l -> m (Maybe l)) -> (l -> m (Vector Integer)) -> Double -> l -> m (Maybe l)
excisableM getParent getFrequency sig l = do
  as <- getAncestorsM getParent l
  lf <- getFrequency l
  go lf as
  where
    go :: Vector Integer -> [l] -> m (Maybe l)
    go _     [] = pure Nothing
    go f (a:as) = do
      af <- getFrequency a
      case Prob.matchesFreqsAsDists sig f af of
        Significant    -> pure (Just a)
        NotSignificant -> go f as



