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
module CSSR.Algorithm.Phase3 where

import qualified Data.Tree.Looping as Looping (Tree)
import qualified Data.MTree.Looping as Looping
import qualified Data.Vector as V
import qualified Data.MTree.Parse as M
import qualified Data.Tree.Parse as P
import qualified Data.Tree.Hist as Hist
import qualified Data.MTree.Parse as MHist
import Debug.Trace

type Terminal = Looping.MLeaf

stepFromTerminal :: Looping.MTree s -> Terminal s -> [Maybe (Looping.Leaf s)]
stepFromTerminal ltree term = foldr go mempty $ zip [0..] (distribution term)
  where
    w :: HashSet Hist.Leaf
    w = HS.toList (histories term)

    alphabet :: Alphabet
    alphabet = alphabet ltree

    navigateToNext :: Int -> Maybe (Looping.Leaf s)
    navigateToNext i = navigateLoopingTree ltree (w :+ alphabet ! i)

    go :: [Maybe (Looping.Leaf s)] -> (Int, Float) -> [Maybe (Looping.Leaf s)]
    go acc (i, p)
       | p > 0     = navigateToNext i : acc
       | otherwise = acc

type Transitions s = HashMap (Terminal s) (HashSet (Looping.MLeaf s))

refine :: Looping.MTree s -> ST s ()
refine ltree' = do
  (stillDirty, transitions) <- findDirt
  if stillDirty
  then refine ltree'
  else pure ()
  where
    toCheck :: Set (Looping.MLeaf s, Looping.MLeaf s)
    toCheck = HS.fromList $ filter (isJust . snd) foo
      where
        foo :: [(Terminal s, Looping.Leaf s)]
        foo = catMaybes $ mapM (\t -> (,t) <$> stepFromTerminal ltree t) (terminals ltree)

    findDirt :: ST s (Bool, Transitions s)
    findDirt = foldrM go' False
      where
        go' :: (Bool, Transitions s) -> (Terminal s, Looping.MLeaf s) -> ST s (Bool, Transitions s)
        go' (dirt, transitions) (term, step) = do
          -- check to see if this leaf is _not_ a terminal leaf
          if terminals ltree `HS.contains` step
          then pure (dirt, HM.insertWith (\new old -> old) term mempty transisions)
          else case step of
            Right subtree -> 
              -- FIXME: "if either of the above return None, else we have a sub-looping-tree and return Some.
              -- refine subtree"
              if isNothing (terminalReference step)
              then do
                _ <-
                  mapM (refineWith term) $
                    filter (not . HS.elem (terminals ltree)) $ -- terminal nodes cannot be overwritten
                      collectLeaves ltree step
                -- loop is now dirty
                pure (True, transitions)
              else
                pure (dirt, transitions)

            Left loop -> do
              -- if we find a "terminal-looping" node (ie- any looping node) that is not a terminal node:
              --   | if it loops to a terminal node: merge this node into the terminal node
              --   | else: make its value a terminal node
              -- if we find a "terminal-edgeSet" node: merge this node into the terminal node
              let isAlreadyRefined = isJust $ terminalReference loop
              if isAlreadyRefined
              then do
                -- merge this node into the terminal node
                let terminal = fromJust (terminalReference loop)
                _ <- addHistories terminal (histories loop)
                _ <- setTerminalReference loop terminal -- FIXME: seems unnessecary
                -- no refinement needed, continue with current isDirty value
                pure (dirt, terminals)

              else do -- we have a non-terminating loop
                -- FIXME: merge the loop's empirical observations as well - but maybe we should do this above...
                -- ...at any rate, the loop is all we need to prototype this.
                _ <- setTerminalReference loop loop -- FIXME: seems unnessecary
                _ <- addTerminalToTree (terminals ltree) loop

                _ <-
                  mapM (refineWith step) $
                    filter (not . HS.elem (terminals ltree)) $ -- terminal nodes cannot be overwritten
                      collectLeaves ltree step
                -- loop is now dirty
                pure (True, transitions)

      transitionGroups :: Transitions s -> [Set (Terminal s)]
      transitionGroups transitions = undefined
        -- // group by transitions
        -- .groupBy{ _._2 }
        -- // throw away transitions, look only at grouped terminals
        -- .mapValues(_.keySet)
        -- .values
        -- // split groups by matching distribution

      toMerge :: ST s _
      toMerge = mapM go transitionGroups
        where
          go :: Set (Terminal s) -> ST s _
          go (sortBy observed . toList -> ts) = do -- TODO: Sorting for debugging, but remove?
            let 
              h = head ts
              t = tail ts
              hset = HS.singleton (head ts)
            _ <- setEdgeSet h hset
            -- just in case there are actually multiple edgsets found with matching transitions
            let
              edgeSets :: ST s (Set[Set[Terminal]])
              edgeSets = foldrM (HS.singleton hset) $ \ess t -> do
                matchFound <- newSTRef False
                forM (filter (not . (const $ readSTRef matchFound) !matchFound && Tree.matches(t)(es.head))
              for (es <- ess if !matchFound && Tree.matches(t)(es.head)) {
                es += t
                t.edgeSet = Some(es)
                matchFound = true
              }

              if (matchFound) ess else {
                val newSet = mutable.Set(t)
                t.edgeSet = Some(newSet)
                ess ++ Set(newSet)
              }
            })
            .map(_.toSet)


          (, tail ts)


          tSet => {
            -- val (head, tail) = unsafeHeadAnd(tSet.toList.sortBy(_.observed.mkString(ltree.alphabet.delim)))
            -- val newSet = mutable.Set(head)
            -- head.edgeSet = Some(newSet)

                        edgeSets
          }
        }
        -- don't look at singleton groups
        .filter{ _.size > 1 }


      // perform final updates and merges
      toMerge
        .foreach {
          set:Set[Terminal] =>
            // holy moly we need to turn this into a dag and not a cyclic-linked-list-tree
            val (head:Terminal, tail:List[Terminal]) = unsafeHeadAnd(set.filter{_.parent.nonEmpty}.toList.sortBy(_.observed.mkString(ltree.alphabet.delim)))
            tail.foreach {
              node =>
                node.parent.get.children.put(node.observation, Left(head))
                ltree.terminals = ltree.terminals - node
            }
        }

      stillDirty = stillDirty || toMerge.nonEmpty

    } while (stillDirty)
  }

--   def headAnd [T] (l:List[T]):(Option[T], List[T]) = (l.headOption, l.tail)
--   def unsafeHeadAnd [T] (l:List[T]):(T, List[T]) = (l.head, l.tail)

--   def collect(ptree: ParseTree, ltree:LoopingTree, depth:Int, states:Set[State], stateMap: Map[Terminal, State]):Unit = {
--     val collectables = ptree.getDepth(depth) ++ ptree.getDepth(depth - 1)

--     collectables
--       .foreach { pLeaf => {
--         val maybeLLeaf = ltree.navigateToTerminal(pLeaf.observed, states.flatMap(_.terminals).toSet)
--         val maybeState = maybeLLeaf.flatMap { terminal => stateMap.get(terminal) }

--         maybeState.foreach { state => state.addHistory(pLeaf) }
--       } }
--   }
-- }

