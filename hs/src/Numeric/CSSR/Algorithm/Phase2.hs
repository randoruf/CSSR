-------------------------------------------------------------------------------
-- | == Phase II: "Growing a Looping Tree" algorithm
--
-- INIT root looping node
-- INIT queue of active, unchecked nodes
-- QUEUE root
-- WHILE queue is not empty
--   DEQUEUE first looping node from the queue
--   COMPUTE homogeneity(dequeued looping node, parse tree)
--   IF node is homogeneous
--   THEN continue
--   ELSE
--     CONSTRUCT new looping nodes for all valid children (one for each symbol in
--               alphabet - must have empirical observation in dataset).
--     FOR each new node constructed
--       COMPUTE excisability(node, looping tree)
--       ADD all new looping nodes to children of active node (mapped by symbol)
--       ADD unexcisable children to queue (FIXME: what about edgesets?)
--   ENDIF
-- ENDWHILE
-------------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Numeric.CSSR.Algorithm.Phase2 where

import qualified Data.Sequence as S
import qualified Data.HashTable.Class as H

import CSSR.Prelude.Mutable
import Data.MTree.Looping as ML

import qualified CSSR.Prelude.Vector as V
import qualified Data.List.Set as Set
import qualified Data.Tree.Conditional as Cond


grow :: forall s . Double -> Cond.Tree -> ST s (ML.MTree s)
grow sig (Cond.Tree _ a hRoot) = do
  rt <- ML.mkRoot hRoot                -- INIT root looping node
  ts <- newSTRef $ fromList [rt]       -- INIT queue of active, unchecked nodes
  let q = S.singleton rt               -- QUEUE root
  findTerminals sig q ts
  pure $ ML.MTree ts rt


-- WHILE queue is not empty
--   DEQUEUE first looping node from the queue
--   COMPUTE homogeneity(dequeued looping node, parse tree)
--   IF node is homogeneous
--   THEN continue
--   ELSE
--     CONSTRUCT new looping nodes for all valid children (one for each symbol in
--               alphabet - must have empirical observation in dataset).
--     FOR each new node constructed
--       COMPUTE excisability(node, looping tree)
--       ADD all new looping nodes to children of active node (mapped by symbol)
--       ADD unexcisable children to queue (FIXME: what about edgesets?)
--   ENDIF
-- ENDWHILE
findTerminals :: forall s . Double -> Seq (MLeaf s) -> STRef s (Set.ListSet (MLeaf s)) -> ST s ()
findTerminals sig q termsRef =
  unless (S.null q) $ do
    let (active, next) = splitTerminals q
    isHomogeneous sig active >>= \case
      True  -> findTerminals sig next termsRef
      False -> do
        cs <- childsToLoops active
        forM_ cs $ uncurry (H.insert (ML.children active))
        let cs'' = rights . map snd $ cs
        modifySTRef termsRef ((`Set.union` fromList cs'') . Set.delete active)
        findTerminals sig (next <> S.fromList cs'') termsRef
  where
    childsToLoops :: MLeaf s -> ST s [(Event, MLNode s)]
    childsToLoops = nextChilds >=> mapM (\(e,c) -> (e,) <$> findLoops c)

    splitTerminals :: Seq (MLeaf s) -> (MLeaf s, Seq (MLeaf s))
    splitTerminals = ((`S.index` 0) *** identity) . S.splitAt 1

    -- COMPUTE excisability(node, looping tree)
    findLoops :: MLeaf s -> ST s (MLNode s)
    findLoops ll = maybe (Right ll) Left <$> ML.excisable sig ll


-- CONSTRUCT new looping nodes for all valid children
--    (one for each symbol in alphabet - must have empirical
--    observation in dataset).
nextChilds :: forall s . MLeaf s -> ST s [(Event, MLeaf s)]
nextChilds active = (ML.childHistories >=> mapM activeAsLeaf . groupHistory) active
  where
    activeAsLeaf :: (Event, [Cond.Leaf]) -> ST s (Event, MLeaf s)
    activeAsLeaf (e, hs) = (e,) <$> ML.mkLeaf (Just active) e hs

    groupHistory :: [Cond.Leaf] -> [(Event, [Cond.Leaf])]
    groupHistory = groupBy (fromMaybe "" . V.head . view (Cond.bodyL . Cond.obsL))

