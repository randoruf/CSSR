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

import qualified Data.Tree.Looping  as Looping (Tree)
import qualified Data.MTree.Looping as Looping
import qualified Data.MTree.Parse   as M
import qualified Data.Tree.Parse    as P
import qualified Data.Tree.Hist     as Hist
import qualified Data.MTree.Parse   as MHist

import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V


type Terminal = Looping.MLeaf


stepFromTerminal :: forall s . Alphabet -> Looping.MTree s -> Terminal s -> ST s [Maybe (Looping.MLNode s)]
stepFromTerminal alpha (Looping.root->rt) term = foldrM go [] $ zip [0..] (toList (distribution term))
  where
    w :: Vector Event
    w = view (_Just . Hist.bodyL . Hist.obsL) -- get the observed history from the representative history
      . minimumBy ordering . HS.toList        -- get the (ASSUMPTION) leaf with the least-sufficiency
      $ Looping.histories term                -- get all histories from the terminal node
      where
        ordering :: Hist.Leaf -> Hist.Leaf -> Ordering
        ordering = compare `on` (length . view (Hist.bodyL . Hist.obsL))

    navigateToNext :: Int -> ST s (Maybe (Looping.MLNode s))
    navigateToNext i = do
      mnext <- Looping.walk (Right rt) (w `V.snoc` (idxToSym alpha ! i))
      pure mnext

    go :: (Int, Double) -> [Maybe (Looping.MLNode s)] -> ST s [Maybe (Looping.MLNode s)]
    go (i, p) acc
       | p > 0     = navigateToNext i >>= \n -> pure (n : acc)
       | otherwise = pure acc


type Transitions s = HashMap (Terminal s) (HashSet (Looping.MLNode s))

toCheck :: forall s . Alphabet -> Looping.MTree s -> ST s [(Looping.MLeaf s, Looping.MLNode s)]
toCheck a tree = terminals tree >>= foldrM go mempty
  where
    go :: Terminal s -> [(Terminal s, Looping.MLNode s)] -> ST s [(Terminal s, Looping.MLNode s)]
    go t memo
      = (memo <>)  -- add to the current list of teminals to check
      . fmap (t,)  -- make sure transitions are paired with their origin terminal
      . catMaybes  -- remove any terminals which truly terminate
      <$> stepFromTerminal a tree t

    terminals :: Looping.MTree s -> ST s [Looping.MLeaf s]
    terminals = fmap HS.toList . readSTRef . Looping.terminals

collectLeaves :: Looping.MTree s -> Looping.MLeaf s -> HashSet (Looping.MLeaf s)
collectLeaves ltree leaf = undefined

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
refine :: forall s . Alphabet -> Looping.MTree s -> ST s ()
refine a ltree = do
  ts <- readSTRef $ Looping.terminals ltree
  check <- toCheck a ltree
  (stillDirty, transitions) <- foldrM (go ts) (False, mempty) check
  when stillDirty (refine a ltree)
  where
    inTerminals :: Looping.MLNode s -> HashSet (Terminal s) -> Bool
    inTerminals = HS.member . either identity identity

    -- IF wa leads to a terminal node
    -- THEN store terminal's transition in transition map
    storeTransitions :: HashSet (Terminal s) -> Bool -> Transitions s -> Terminal s -> Looping.MLeaf s -> ST s (Bool, Transitions s)
    storeTransitions ts isDirty tmap term subtree = Looping.getTermRef subtree
      >>= \case
        Just _  -> pure (isDirty, tmap)
        Nothing -> do -- loop is now dirty
          runRefinement ts term subtree
          pure (True, tmap)

    -- terminal nodes cannot be overwritten
    runRefinement :: HashSet (Terminal s) -> Terminal s -> Looping.MLeaf s -> ST s ()
    runRefinement ts term step
      = mapM_ (`Looping.setTermRef` term)
      . filter (not . (`HS.member` ts))
      . HS.toList
      $ collectLeaves ltree step

    go :: HashSet (Terminal s) -> (Terminal s, Looping.MLNode s) -> (Bool, Transitions s) -> ST s (Bool, Transitions s)
    go ts (term, step) (dirt, transitions)
      | step `inTerminals` ts = pure (dirt, HM.insertWith (\new old -> old) term mempty transitions)
      | otherwise             = either (storeTransitions ts dirt transitions term) refineSubtree step
      where
        -- FIXME: "if either of the above return None, else we have a sub-looping-tree and return Some.
        -- refine subtree"
        addTerminalToTree = undefined
        addHistories = undefined

        -- ELSEIF wa does not lead to a terminal node
        --   copy the sub-looping-tree rooted at (the node reached by) wa to t,
        --       giving all terminal nodes the predictive distribution of t
        --   store terminal's transition in transition map
        --   - continue
        refineSubtree :: Looping.MLeaf s -> ST s (Bool, Transitions s)
        refineSubtree loop = Looping.getTermRef loop >>= \case
          -- if we find a "terminal-looping" node (ie- any looping node) that is not a terminal node:
          -- FIXME: if we find a "terminal-edgeSet" node: merge this node into the terminal node
          Just t -> do                                     -- is already refined
            _ <- addHistories t (Looping.histories loop)   -- merge this node into the terminal node
            loop `Looping.setTermRef` t                    -- FIXME: seems unnessecary
            pure (dirt, transitions)                       -- continue with current isDirty value

          Nothing -> do                                    -- we have a non-terminating loop
            loop `Looping.setTermRef` term                 -- FIXME: seems unnessecary
            void $ addTerminalToTree (Looping.terminals ltree) loop
            -- FIXME: merge the loop's empirical observations as well - but maybe we should do this above...
            -- ...at any rate, the loop is all we need to prototype this.
            runRefinement ts term loop
            pure (True, transitions)

