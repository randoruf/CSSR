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
import qualified Data.Text as T
import qualified Data.Tree.Hist as Hist
import qualified Data.Sequence as S
import qualified Data.List.Set as Set
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
  ts' <- newSTRef =<< fromList <$> readSTRef ts
  pure $ ML.MTree ts' rt
  where
    go :: Seq (MLeaf s) -> STRef s [MLeaf s] -> ST s ()
    go q termsRef = do
      t <- readSTRef termsRef
      pure ()
      {-
      unless (S.null q) $ do                                  -- WHILE queue is not empty
        terms <- readSTRef termsRef
        isHomogeneous sig active >>= \case                    --   COMPUTE homogeneity(dequeued looping node, parse tree)
          True  -> go next termsRef                           --   IF node is homogeneous
                                                              --   THEN continue
          False -> do                                         --   ELSE
            cs' <- nextChilds                                 --     CONSTRUCT new looping nodes for all valid children (one for each symbol in
                                                              --               alphabet - must have empirical observation in dataset).
            let cs'' = fmap snd cs'
            cs <- forM cs' $                                  --     FOR each new node constructed
              \(e, x) -> findLoops x >>= (pure . (e,))        --       COMPUTE excisability(node, looping tree)
            forM_ cs $
              uncurry (H.insert (ML.children active))         --       ADD all new looping nodes to children of active node (mapped by symbol)
            writeSTRef termsRef (cs'' <> delete active terms) --       ADD unexcisable children to queue (FIXME: what about edgesets?)
                                                              --   ENDIF
            go (next <> S.fromList cs'') termsRef             -- ENDWHILE
      -}
      where
        (active, next) = splitTerminals q

        splitTerminals :: Seq (MLeaf s) -> (MLeaf s, Seq (MLeaf s))
        splitTerminals = ((`S.index` 0) *** identity) . S.splitAt 1

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
            activeHistories = fmap HS.toList . readSTRef . ML.histories $ active

            activeAsLeaf :: [Hist.Leaf] -> ST s (MLeaf s)
            activeAsLeaf _hs = ML.mkLeaf (Just active) _hs

            groupHistory :: [Hist.Leaf] -> [(Event, [Hist.Leaf])]
            groupHistory = groupBy (V.head . view (Hist.bodyL . Hist.obsL))


findTerminals :: Double -> MLeaf s -> Seq (MLeaf s) -> STRef s [MLeaf s] -> ST s ()
findTerminals sig active q termsRef = do
  let (active, next) = splitTerminals q
  t <- readSTRef termsRef
  unless (S.null q) $ do                                  -- WHILE queue is not empty
    terms <- readSTRef termsRef
    isHomogeneous sig active >>= \case                    --   COMPUTE homogeneity(dequeued looping node, parse tree)
      True -> findTerminals sig active next termsRef      --   IF node is homogeneous
                                                          --   THEN continue
      False -> do                                         --   ELSE
        cs' <- nextChilds active                          --     CONSTRUCT new looping nodes for all valid children (one for each symbol in
                                                          --               alphabet - must have empirical observation in dataset).
        let cs'' = fmap snd cs'
        cs <- forM cs' $                                  --     FOR each new node constructed
          \(e, x) -> findLoops sig x >>= (pure . (e,))        --       COMPUTE excisability(node, looping tree)
        forM_ cs $
          uncurry (H.insert (ML.children active))         --       ADD all new looping nodes to children of active node (mapped by symbol)
        writeSTRef termsRef (cs'' <> delete active terms) --       ADD unexcisable children to queue (FIXME: what about edgesets?)
                                                          --   ENDIF
        findTerminals sig active (next <> S.fromList cs'') termsRef  -- ENDWHILE


queueRoot :: forall s . Double -> Hist.Tree -> ST s (MLeaf s, STRef s (Set.ListSet (MLeaf s)), Seq (MLeaf s))
queueRoot sig (Hist.Tree _ a hRoot) = do
  rt <- ML.mkRoot a hRoot   -- INIT root looping node
  ts <- newSTRef $ fromList [rt]       -- INIT queue of active, unchecked nodes
  let q = S.singleton rt    -- QUEUE root
  return (rt, ts, q)


splitTerminals :: Seq (MLeaf s) -> (MLeaf s, Seq (MLeaf s))
splitTerminals = ((`S.index` 0) *** identity) . S.splitAt 1


-- COMPUTE excisability(node, looping tree)
findLoops :: Double -> MLeaf s -> ST s (MLNode s)
findLoops sig ll =
  ML.excisable sig ll >>= \case
    Nothing -> return $ Right ll
    Just ex -> return $ Left ex


-- CONSTRUCT new looping nodes for all valid children
--    (one for each symbol in alphabet - must have empirical
--    observation in dataset).
nextChilds :: forall s . MLeaf s -> ST s [(Event, MLeaf s)]
nextChilds active = do
  hs <- readSTRef $ ML.histories active
  xs <- ML.childHistories $ active
  -- traceM $ showHists xs <> " parents: " <> showHists (HS.toList hs)
  let ys' = groupHistory xs
  -- traceM $ show $ map (\(e, ls) -> (e, showHists ls)) $ ys'
  mapM activeAsLeaf ys'
  where
    activeAsLeaf :: (Event, [Hist.Leaf]) -> ST s (Event, MLeaf s)
    activeAsLeaf (e, _hs) = (e,) <$> ML.mkLeaf (Just active) _hs

    groupHistory :: [Hist.Leaf] -> [(Event, [Hist.Leaf])]
    groupHistory = groupBy (fromMaybe "" . vHead . view (Hist.bodyL . Hist.obsL))

    vHead :: Vector a -> Maybe a
    vHead v =
      if V.null v
      then Nothing
      else Just (V.head v)


test1 :: Double -> Hist.Tree -> ST s L.Tree
test1 sig htree = do
  (rt, ts, q) <- queueRoot sig htree
  findTerminals_ sig q ts
  cs::[(Event,MLNode s)] <- H.toList (ML.children rt)
  let go :: (Event, MLNode s) -> ST s Int
      go (e, n) = do
        case n of
          Left loop -> pure (-1)
          Right c -> do
            hs' <- readSTRef (ML.histories c)
            pure . HS.size $ hs'

  hls :: [Int] <- mapM go cs
  traceM $ "hls : " <> show hls

  -- ts' <- newSTRef =<< readSTRef ts
  t <- ML.freezeTree $ ML.MTree ts rt
  pure t

maine :: IO ()
maine = do
  con <- readFile "../../test-machines/tmp/ep/EP_timeseq"
  let hs = initialization 5 con
  let x = runST $ test1 0.01 hs
  print x

findTerminals_ :: Double -> Seq (MLeaf s) -> STRef s (Set.ListSet (MLeaf s)) -> ST s ()
findTerminals_ sig q termsRef = do
  unless (S.null q) $ do
    let (active, next) = splitTerminals q
    terms <- readSTRef termsRef
    hs <- readSTRef $ ML.histories active
    isHomogeneous sig active >>= \case
      True -> do
        -- traceM $ "node " ++ showHists hs ++ " is homogeneous: " ++ showHDists hs ++ ", childs: <>"
        findTerminals_ sig next termsRef
      False -> do
        --traceM $ "node " ++ showHists hs ++ " is not homogeneous: " ++ showHDists hs ++ ", childs: <>"
        let (active, next) = splitTerminals q

        cs' :: [(Event,  MLeaf s)] <- nextChilds active
        cs  :: [(Event, MLNode s)] <- mapM (\(e, x) -> (e,) <$> findLoops sig x) cs'
        forM_ cs $ uncurry (H.insert (ML.children active))

        let cs'' = rights . map snd $ cs
        modifySTRef termsRef (\ls -> (Set.delete active ls) `Set.union` (fromList cs''))
        -- findTerminals_ sig (next <> S.fromList cs'') termsRef  -- ENDWHILE

-- def grow(tree:ParseTree):LoopingTree = {
--   val ltree = new LoopingTree(tree)
--   val activeQueue = ListBuffer[LLeaf](ltree.root)
--   val findAlternative = LoopingTree.findAlt(ltree)(_)
--
--   while (activeQueue.nonEmpty) {
--     val active:LLeaf = activeQueue.remove(0)
--
--     val isHomogeneous:Boolean = active.histories.forall{ LoopingTree.nextHomogeneous(tree) }
--
--     if (isHomogeneous) {
--       debug("we've hit our base case")
--     } else {
--
--       val nextChildren:Map[Event, LoopingTree.Node] = active.histories
--         .flatMap { _.children }
--         .groupBy{ _.observation }
--         .map { case (c, pleaves) => {
--           val lleaf:LLeaf = new LLeaf(c +: active.observed, pleaves, Option(active))
--           val alternative:Option[LoopingTree.AltNode] = findAlternative(lleaf)
--           c -> alternative.toRight(lleaf)
--         } }
--
--       active.children ++= nextChildren
--       // Now that active has children, it cannot be considered a terminal node. Thus, we elide the active node:
--       ltree.terminals = ltree.terminals ++ LoopingTree.leafChildren(nextChildren).toSet[LLeaf] - active
--       // FIXME: how do edge-sets handle the removal of an active node? Also, are they considered terminal?
--       activeQueue ++= LoopingTree.leafChildren(nextChildren)
--     }
--   }
--
--   ltree
-- }


showHists :: [Hist.Leaf] -> String
showHists = show . fmap (T.concat . V.toList . Hist.obs . Hist.body)

showHDists :: HashSet Hist.Leaf -> String
showHDists = show . fmap (intercalate "," . V.toList . fmap f'4 . Prob.freqToDist . Hist.frequency . Hist.body) . HS.toList

