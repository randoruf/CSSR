-------------------------------------------------------------------------------
-- | == Phase III: "Refine the Looping Tree"
--
-- SET change = true
-- UNTIL change == false
--   INIT transition map (terminal -> node)
--
--   // calculate predictive probabilities
--   FOR each terminal node (t)
--     FOR each non-looping path w to t
--       FOR each symbol a in the alphabet follow the path wa in the tree
--         IF wa leads to a terminal node
--         THEN store terminal's transition in transition map
--         ELSEIF wa does not lead to a terminal node
--           copy the sub-looping-tree rooted at (the node reached by) wa to t,
--               giving all terminal nodes the predictive distribution of t
--           store terminal's transition in transition map
--           - continue
--         ELSE
--           ENDFOR (break inner-most loop)
--         ENDIF
--       ENDFOR
--     ENDFOR
--   ENDFOR
--
--   // merge edgesets:
--   COLLECT map of (terminal nodes -> edgesets)
--     FILTER terminals in transition map keys where terminal.isEdgeSet == true
--     GROUPBY a terminal's distribution and the value from the transition map
--
--
--   FOR each terminal node belonging to an edgesets
--     SET terminal.edgeset to the corresponding set from the map
--         of (terminal nodes -> edgesets)
--
--   IF map of (terminal nodes -> edgesets) is nonEmpty
--   THEN set change = true
--   ENDIF
--
-- ENDUNTIL
--
-- Questions:
--   + if we let a terminal node's distribution override another terminal node's
--       distribution (via subtree) will order matter?
-------------------------------------------------------------------------------
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CSSR.Algorithm.Phase3 where

import Data.Alphabet
import CSSR.Prelude.Mutable
import CSSR.Probabilistic

import qualified Data.MTree.Looping as L
import qualified Data.MTree.Parse   as M
import qualified Data.Tree.Parse    as P
import qualified Data.Tree.Hist     as Hist
import qualified Data.MTree.Parse   as MHist

import Data.List.Set (ListSet)
import qualified Data.List.Set as S
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import Data.Map (Map)
import qualified Data.Map as Map



type Terminal = L.MLeaf
type Transitions s = ListSet (Terminal s, ListSet (L.MLNode s))


stepFromTerminal :: forall s . Alphabet -> L.MTree s -> Terminal s -> ST s [Maybe (L.MLNode s)]
stepFromTerminal alpha (L.root->rt) term = do
  dist <- toList . freqToDist <$> readSTRef (L.frequency term)
  foldrM go [] $ zip [0..] dist
  where
    w' :: ST s (Vector Event)
    w' = view (_Just . Hist.bodyL . Hist.obsL) -- get the observed history from the representative history
      . minimumBy ordering . HS.toList        -- get the (ASSUMPTION) leaf with the least-sufficiency
      <$> readSTRef (L.histories term)        -- get all histories from the terminal node
      where
        ordering :: Hist.Leaf -> Hist.Leaf -> Ordering
        ordering = compare `on` (length . view (Hist.bodyL . Hist.obsL))

    navigateToNext :: Int -> ST s (Maybe (L.MLNode s))
    navigateToNext i = do
      w <- w'
      mnext <- L.walk (Right rt) (w `V.snoc` (idxToSym alpha ! i))
      pure mnext

    go :: (Int, Double) -> [Maybe (L.MLNode s)] -> ST s [Maybe (L.MLNode s)]
    go (i, p) acc
       | p > 0     = navigateToNext i >>= \n -> pure (n : acc)
       | otherwise = pure acc


toCheck :: forall s . Alphabet -> L.MTree s -> ST s [(L.MLeaf s, L.MLNode s)]
toCheck a tree = terminals tree >>= foldrM go mempty
  where
    go :: Terminal s -> [(Terminal s, L.MLNode s)] -> ST s [(Terminal s, L.MLNode s)]
    go t memo
      = (memo <>)  -- add to the current list of teminals to check
      . fmap (t,)  -- make sure transitions are paired with their origin terminal
      . catMaybes  -- remove any terminals which truly terminate
      <$> stepFromTerminal a tree t

    terminals :: L.MTree s -> ST s (ListSet (L.MLeaf s))
    terminals = readSTRef . L.terminals

collectLeaves :: L.MTree s -> L.MLeaf s -> ListSet (L.MLeaf s)
collectLeaves tree leaf = undefined

-- SET change = true
-- UNTIL change == false
--   INIT transition map (terminal -> node)
--
--   FOR each terminal node (t)
--     FOR each non-looping path w to t
--       FOR each symbol a in the alphabet follow the path wa in the tree
--         IF wa leads to a terminal node
--         THEN store terminal's transition in transition map
--         ELSEIF wa does not lead to a terminal node
--           copy the sub-looping-tree rooted at (the node reached by) wa to t,
--               giving all terminal nodes the predictive distribution of t
--           store terminal's transition in transition map
--           - continue
--         ELSE
--           ENDFOR (break inner-most loop)
--         ENDIF
--       ENDFOR
--     ENDFOR
--   ENDFOR
refine :: forall s . Alphabet -> L.MTree s -> ST s ()
refine a tree = do
  ts <- readSTRef $ L.terminals tree
  check <- toCheck a tree
  (stillDirty, transitions) <- foldrM (go ts) (False, mempty) check
  when stillDirty (refine a tree)
  where
    inTerminals :: L.MLNode s -> ListSet (Terminal s) -> Bool
    inTerminals = S.member . either identity identity

    -- IF wa leads to a terminal node
    -- THEN store terminal's transition in transition map
    storeTransitions :: ListSet (Terminal s) -> Bool -> Transitions s -> Terminal s -> L.MLeaf s -> ST s (Bool, Transitions s)
    storeTransitions ts isDirty tmap term subtree = L.getTermRef subtree
      >>= \case
        Just _  -> pure (isDirty, tmap)
        Nothing -> do -- loop is now dirty
          runRefinement ts term subtree
          pure (True, tmap)

    -- terminal nodes cannot be overwritten
    runRefinement :: ListSet (Terminal s) -> Terminal s -> L.MLeaf s -> ST s ()
    runRefinement ts term step
      = mapM_ (`L.setTermRef` term)
      . filter (not . (`S.member` ts))
      . S.toList
      $ collectLeaves tree step

    go :: ListSet (Terminal s) -> (Terminal s, L.MLNode s) -> (Bool, Transitions s) -> ST s (Bool, Transitions s)
    go ts (term, step) (dirt, transitions)
      | step `inTerminals` ts = pure (dirt, S.insertAssocWith (\new old -> old) term (S.singleton step) transitions)
      | otherwise             = either (storeTransitions ts dirt transitions term) refineSubtree step
      where
        -- FIXME: "if either of the above return None, else we have a sub-looping-tree and return Some.
        -- refine subtree"

        -- ELSEIF wa does not lead to a terminal node
        --   copy the sub-looping-tree rooted at (the node reached by) wa to t,
        --       giving all terminal nodes the predictive distribution of t
        --   store terminal's transition in transition map
        --   - continue
        refineSubtree :: L.MLeaf s -> ST s (Bool, Transitions s)
        refineSubtree loop = L.getTermRef loop >>= \case
          -- if we find a "terminal-looping" node (ie- any looping node) that is not a terminal node:
          -- FIXME: if we find a "terminal-edgeSet" node: merge this node into the terminal node

          Just t -> do                               -- is already refined
            readSTRef (L.histories loop) >>= L.addHistories t       -- merge this node into the terminal node
            loop `L.setTermRef` t                    -- FIXME: seems unnessecary
            pure (dirt, transitions)                 -- continue with current isDirty value

          Nothing -> do                              -- we have a non-terminating loop
            loop `L.setTermRef` term                 -- FIXME: seems unnessecary
            tree `L.addTerminal` loop
            -- FIXME: merge the loop's empirical observations as well - but maybe we should do this above...
            -- ...at any rate, the loop is all we need to prototype this.
            runRefinement ts term loop
            pure (True, transitions)

