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

import CSSR.Prelude
import Data.MTree.Looping as ML
import Data.Maybe
import Data.Tree.Looping as L hiding (excisable, homogeneous)
import qualified CSSR.Prelude.Vector as V
import qualified CSSR.Probabilistic as Prob
import qualified Data.Text as T
import qualified Data.Tree.Hist as Hist
import qualified Data.Sequence as S
import qualified Data.List.Set as Set
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.Class as H

import CSSR.Prelude.Mutable
-- for doctest
import CSSR.Algorithm.Phase1


-- |
-- Example:
--
-- >>> import CSSR.Algorithm.Phase1
-- >>> let short_ep = "00011110001100011110000111101101111111111000110001101101100111100111100"
-- >>> let htree = initialization 1 short_ep
-- >>> runST $ grow 0.01 htree >>= ML.freezeTree
-- Leaf{fromList [
--      " "->Leaf{obs: [], freq: [28,42]}
--           children:
--           "0"->Leaf{obs: ["0"], freq: [1,1], no children}
-- <BLANKLINE>
--           "1"->Leaf{obs: ["1"], freq: [1,1], no children}], [0,0], fromList []}
grow :: forall s . Double -> Hist.Tree -> ST s (ML.MTree s)
grow sig htree = do
  (rt, ts, q) <- queueRoot sig htree
  findTerminals sig q ts
  pure $ ML.MTree ts rt


-- INIT root looping node
-- INIT queue of active, unchecked nodes
-- QUEUE root
queueRoot :: forall s . Double -> Hist.Tree -> ST s (MLeaf s, STRef s (Set.ListSet (MLeaf s)), Seq (MLeaf s))
queueRoot sig (Hist.Tree _ a hRoot) = do
  rt <- ML.mkRoot a hRoot   -- INIT root looping node
  ts <- newSTRef $ fromList [rt]       -- INIT queue of active, unchecked nodes
  let q = S.singleton rt    -- QUEUE root
  return (rt, ts, q)


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
findTerminals sig q termsRef = do
  unless (S.null q) $ do
    let (active, next) = splitTerminals q
    terms <- readSTRef termsRef
    hs <- readSTRef $ ML.histories active
    isHomogeneous sig active >>= \case
      True  -> findTerminals sig next termsRef
      False -> do
        cs <- childsToLoops active
        forM_ cs $ uncurry (H.insert (ML.children active))
        let cs'' = rights . map snd $ cs
        modifySTRef termsRef ((`Set.union` fromList cs'') . (Set.delete active))
        findTerminals sig (next <> S.fromList cs'') termsRef
  where
    childsToLoops :: MLeaf s -> ST s [(Event, MLNode s)]
    childsToLoops = nextChilds >=> mapM ((>$>) findLoops)

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
    activeAsLeaf :: (Event, [Hist.Leaf]) -> ST s (Event, MLeaf s)
    activeAsLeaf s = ML.mkLeaf (Just active) >$> s

    groupHistory :: [Hist.Leaf] -> [(Event, [Hist.Leaf])]
    groupHistory = groupBy (fromMaybe "" . V.head . view (Hist.bodyL . Hist.obsL))


-- ========================================================================= --

test1 :: Double -> Hist.Tree -> ST s L.Tree
test1 sig htree = do
  (rt, ts, q) <- queueRoot sig htree
  findTerminals sig q ts
  cs::[(Event,MLNode s)] <- H.toList (ML.children rt)
  let go :: (Event, MLNode s) -> ST s Int
      go (e, n) =
        case n of
          Left loop -> pure (-1)
          Right c -> do
            hs' <- readSTRef (ML.histories c)
            pure . HS.size $ hs'

  hls :: [Int] <- mapM go cs
  traceM $ "hls : " <> show hls

  t <- ML.freezeTree $ ML.MTree ts rt
  pure t

maine :: IO ()
maine = do
  con <- readFile "../../test-machines/tmp/ep/EP_timeseq"
  let hs = initialization 5 con
  let x = runST $ test1 0.01 hs
  print x


