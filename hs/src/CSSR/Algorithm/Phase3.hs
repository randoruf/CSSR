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


stepFromTerminal :: forall s . Alphabet -> Looping.MTree s -> Terminal s -> ST s [Maybe (Looping.MLeaf s)]
stepFromTerminal alpha (Looping.root->rt) term = foldrM go [] $ zip [0..] (toList (distribution term))
  where
    w :: Vector Event
    w = view (_Just . Hist.bodyL . Hist.obsL) lcd
      where
        lcd :: Maybe Hist.Leaf
        lcd = head . sortBy ordering . HS.toList $ Looping.histories term

        ordering :: Hist.Leaf -> Hist.Leaf -> Ordering
        ordering = compare `on` (length . view (Hist.bodyL . Hist.obsL))

    navigateToNext :: Int -> ST s (Maybe (Looping.MLeaf s))
    navigateToNext i = do
      mnext <- Looping.walk (Right rt) (w `V.snoc` (idxToSym alpha ! i))
      pure $ fmap (either identity identity) mnext

    go :: (Int, Double) -> [Maybe (Looping.MLeaf s)] -> ST s [Maybe (Looping.MLeaf s)]
    go (i, p) acc
       | p > 0     = navigateToNext i >>= \n -> pure (n : acc)
       | otherwise = pure acc


type Transitions s = HashMap (Terminal s) (HashSet (Looping.MLeaf s))

toCheck :: forall s . Alphabet -> Looping.MTree s -> ST s [(Looping.MLeaf s, Looping.MLeaf s)]
toCheck a tree = foldrM go mempty (terminals tree)
  where
    go :: Looping.MLeaf s -> [(Looping.MLeaf s, Looping.MLeaf s)] -> ST s [(Looping.MLeaf s, Looping.MLeaf s)]
    go t memo
      = (memo <>)  -- add to the current list of teminals to check
      . fmap (,t)  -- make sure transitions are paired with their origin terminal
      . catMaybes  -- remove any terminals which truly terminate
      <$> stepFromTerminal a tree t

    terminals :: Looping.MTree s -> [Looping.MLeaf s]
    terminals = HS.toList . Looping.terminals


refine :: forall s . Alphabet -> Looping.MTree s -> ST s ()
refine a ltree' = do
  undefined
--   (stillDirty, transitions) <- findDirt
--   when stillDirty $ refine a ltree'
--   where
--     findDirt :: ST s (Bool, Transitions s)
--     findDirt = foldrM go' False
--       where
--         go' :: (Bool, Transitions s) -> (Terminal s, Looping.MLeaf s) -> ST s (Bool, Transitions s)
--         go' (dirt, transitions) (term, step) =
--           -- check to see if this leaf is _not_ a terminal leaf
--           if terminals ltree `S.member` step
--           then pure (dirt, HM.insertWith (\new old -> old) term mempty transisions)
--           else case step of
--             Right subtree -> onRight subtree
--             Left loop -> onLeft loop
--           where
--             -- FIXME: "if either of the above return None, else we have a sub-looping-tree and return Some.
--             -- refine subtree"
--             onRight subtree =
--               case (terminalReference step) of
--                 Nothing -> runRefinement -- loop is now dirty
--                 Just _  -> pure (dirt, transitions)
--
--             runRefinement =
--               mapM (refineWith term) torefine $
--                 filter (not . S.member (terminals ltree)) $
--                   collectLeaves ltree step
--               >> pure (True, transitions)
--               where
--                 -- terminal nodes cannot be overwritten
--                 torefine = filter (not . S.member (terminals ltree)) $ collectLeaves ltree step
--
--
--             onLeft loop =
--               -- if we find a "terminal-looping" node (ie- any looping node) that is not a terminal node:
--               -- FIXME: if we find a "terminal-edgeSet" node: merge this node into the terminal node
--               case terminalReference loop of
--                 Just terminal -> do                             -- is already refined
--                   _ <- addHistories terminal (histories loop)   -- merge this node into the terminal node
--                   _ <- setTerminalReference loop terminal       -- FIXME: seems unnessecary
--                   pure (dirt, terminals)                        -- continue with current isDirty value
--
--                 Nothing -> do                                   -- we have a non-terminating loop
--                   void $ setTerminalReference loop loop         -- FIXME: seems unnessecary
--                   void $ addTerminalToTree (terminals ltree) loop
--                   -- FIXME: merge the loop's empirical observations as well - but maybe we should do this above...
--                   -- ...at any rate, the loop is all we need to prototype this.
--                   runRefinement
--
-- --   def collect(ptree: ParseTree, ltree:LoopingTree, depth:Int, states:Set[State], stateMap: Map[Terminal, State]):Unit = {
-- --     val collectables = ptree.getDepth(depth) ++ ptree.getDepth(depth - 1)
--
-- --     collectables
-- --       .foreach { pLeaf => {
-- --         val maybeLLeaf = ltree.navigateToTerminal(pLeaf.observed, states.flatMap(_.terminals).toSet)
-- --         val maybeState = maybeLLeaf.flatMap { terminal => stateMap.get(terminal) }
--
-- --         maybeState.foreach { state => state.addHistory(pLeaf) }
-- --       } }
-- --   }
-- -- }
--
