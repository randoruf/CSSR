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
import qualified Data.Tree.Hist as Hist (Tree(..), Leaf(..), body, obs, children)
import qualified Data.Sequence as S (singleton, null, fromList, splitAt, index)
import qualified Data.HashSet as HS (fromList)
import qualified Data.HashMap.Strict as HM (elems)
import qualified Data.Vector as V (head)
import qualified Data.HashTable.Class as H (insert, toList)
import qualified CSSR.Probabilistic as Prob (unsafeMatch, unsafeMatch_, frequency)

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
grow :: forall s . Double -> Hist.Tree -> ST s (ML.MLeaf s)
grow sig (Hist.Tree _ a hRoot) = do
  rt <- ML.mkRoot a hRoot   -- INIT root looping node
  ts <- newSTRef [rt]       -- INIT queue of active, unchecked nodes
  let q = S.singleton rt    -- QUEUE root
  go q ts
  return rt
  where
    go :: Seq (MLeaf s) -> STRef s [MLeaf s] -> ST s ()
    go q termsRef =
      unless (S.null q) $ do            -- WHILE queue is not empty
        terms <- readSTRef termsRef
        isHomogeneous sig active >>= \case
          True  -> go next termsRef
          False -> do
            cs' <- nextChilds
            let cs'' = fmap snd cs'

            -- COMPUTE excisability(node, looping tree)
            cs <- (flip mapM) cs' $ \(e, x) -> findLoops x >>= (pure . (e,))

            forM_ cs $ \(e, x) -> H.insert (ML.children active) e x
            writeSTRef termsRef (cs'' <> delete active terms)

            --   ADD all new looping nodes to children of active (mapped by symbol)
            --   ADD unexcisable children to queue (FIXME: what about edgesets?)
            go (next <> S.fromList cs'') termsRef

      where
        findLoops :: MLeaf s -> ST s (MLNode s)
        findLoops ll =
          excisable sig ll >>= \case
            Nothing -> return $ Right ll
            Just ex -> return $ Left ex

        next :: Seq (MLeaf s)
        (active', next) = S.splitAt 1 q

        active :: MLeaf s
        active = S.index active' 0

        -- CONSTRUCT new looping nodes for all valid children
        --    (one for each symbol in alphabet - must have empirical
        --    observation in dataset).
        nextChilds :: ST s [(Event, MLeaf s)]
        nextChilds = do
          hs <- (fmap.fmap) fst . H.toList . ML.histories $ active
          traverse (\(e, _hs) -> (e,) <$> ML.mkLeaf (Just active) _hs) $ groupHistory hs

        groupHistory :: [Hist.Leaf] -> [(Event, [Hist.Leaf])]
        groupHistory = groupBy (V.head . view (Hist.body . Hist.obs))


