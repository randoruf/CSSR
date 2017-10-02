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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module CSSR.Algorithm.Phase2 where


import Data.MTree.Looping as ML
import qualified Data.Tree.Hist as Hist
import qualified Data.Sequence as S
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.HashTable.Class as H
import qualified CSSR.Probabilistic as Prob

import CSSR.Prelude.Mutable
-- for doctest
import CSSR.Algorithm.Phase1


-- |
-- Example:
--
-- >>> import CSSR.Algorithm.Phase1
-- >>> let short_ep = "00011110001100011110000111101101111111111000110001101101100111100111100"
-- >>> let htree = initialization 1 short_ep
-- >>> runST $ grow 0.01 htree >>= ML.freeze
-- Leaf{fromList [
--      " "->Leaf{obs: [], freq: [28,42]}
--           children:
--           "0"->Leaf{obs: ["0"], freq: [1,1], no children}
-- <BLANKLINE>
--           "1"->Leaf{obs: ["1"], freq: [1,1], no children}], [0,0], fromList []}
grow :: forall s . Double -> Hist.Tree -> ST s (ML.MTree s)
grow sig (Hist.Tree _ a hRoot) = do
  rt <- ML.mkRoot a hRoot   -- INIT root looping node
  ts <- newSTRef [rt]       -- INIT queue of active, unchecked nodes
  let q = S.singleton rt    -- QUEUE root
  go q ts
  ts' <- HS.fromList <$> readSTRef ts
  pure $ ML.MTree ts' rt
  where
    go :: Seq (MLeaf s) -> STRef s [MLeaf s] -> ST s ()
    go q termsRef =
      unless (S.null q) $ do                                  -- WHILE queue is not empty
        terms <- readSTRef termsRef
        isHomogeneous sig active >>= \case                    --   COMPUTE homogeneity(dequeued looping node, parse tree)
          True  -> go next termsRef                           --   IF node is homogeneous
                                                              --   THEN continue
          False -> do                                         --   ELSE
            cs' <- nextChilds                                 --     CONSTRUCT new looping nodes for all valid children (one for each symbol in
                                                              --               alphabet - must have empirical observation in dataset).
            let cs'' = fmap snd cs'
            cs <-
              (flip mapM) cs' $                               --     FOR each new node constructed
                \(e, x) -> findLoops x >>= (pure . (e,))      --       COMPUTE excisability(node, looping tree)
            forM_ cs $
              \(e, x) -> H.insert (ML.children active) e x    --       ADD all new looping nodes to children of active node (mapped by symbol)
            writeSTRef termsRef (cs'' <> delete active terms) --       ADD unexcisable children to queue (FIXME: what about edgesets?)
                                                              --   ENDIF
            go (next <> S.fromList cs'') termsRef             -- ENDWHILE
      where
        (active, next) = splitTerminals q

        splitTerminals :: Seq (MLeaf s) -> (MLeaf s, Seq (MLeaf s))
        splitTerminals = ((flip S.index 0) *** identity) . S.splitAt 1

        -- COMPUTE excisability(node, looping tree)
        findLoops :: MLeaf s -> ST s (MLNode s)
        findLoops ll =
          excisable sig ll >>= \case
            Nothing -> return $ Right ll
            Just ex -> return $ Left ex

        -- CONSTRUCT new looping nodes for all valid children
        --    (one for each symbol in alphabet - must have empirical
        --    observation in dataset).
        nextChilds :: ST s [(Event, MLeaf s)]
        nextChilds =
          activeHistories >>=
          traverse (\(e, _hs) -> (e,) <$> activeAsLeaf _hs) . groupHistory
          where
            activeHistories :: ST s [Hist.Leaf]
            activeHistories = (fmap.fmap) fst . H.toList . ML.histories $ active

            activeAsLeaf :: [Hist.Leaf] -> ST s (MLeaf s)
            activeAsLeaf _hs = ML.mkLeaf (Just active) _hs

            groupHistory :: [Hist.Leaf] -> [(Event, [Hist.Leaf])]
            groupHistory = groupBy (V.head . view (Hist.bodyL . Hist.obsL))


