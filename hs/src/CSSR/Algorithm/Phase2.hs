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
    isHomogeneous sig active >>= \case                              --   COMPUTE homogeneity(dequeued looping node, parse tree)
      True -> findTerminals sig active next termsRef     --   IF node is homogeneous
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


queueRoot :: forall s . Double -> Hist.Tree -> ST s (MLeaf s, STRef s [MLeaf s], Seq (MLeaf s))
queueRoot sig (Hist.Tree _ a hRoot) = do
  rt <- ML.mkRoot a hRoot   -- INIT root looping node
  ts <- newSTRef [rt]       -- INIT queue of active, unchecked nodes
  let q = S.singleton rt    -- QUEUE root
  return (rt, ts, q)


splitTerminals :: Seq (MLeaf s) -> (MLeaf s, Seq (MLeaf s))
splitTerminals = ((`S.index` 0) *** identity) . S.splitAt 1


-- COMPUTE excisability(node, looping tree)
findLoops :: Double -> MLeaf s -> ST s (MLNode s)
findLoops sig ll =
  excisable sig ll >>= \case
    Nothing -> return $ Right ll
    Just ex -> return $ Left ex


-- CONSTRUCT new looping nodes for all valid children
--    (one for each symbol in alphabet - must have empirical
--    observation in dataset).
nextChilds :: forall s . MLeaf s -> ST s [(Event, MLeaf s)]
nextChilds active = do
  xs <- ML.childHistories $ active
  ys' <- pure (groupHistory xs)
  ys <- mapM activeAsLeaf ys'
  pure ys
  where
    activeAsLeaf :: (Event, [Hist.Leaf]) -> ST s (Event, MLeaf s)
    activeAsLeaf (e, _hs) = do
      r <- ML.mkLeaf (Just active) _hs
      pure (e, r)

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
  findTerminals_ sig rt q ts
  traceM "...but it's not done!"
  ts' <- newSTRef =<< fromList <$> readSTRef ts
  t <- ML.freezeTree $ ML.MTree ts' rt
  traceM "and now it's done!"
  -- traceM (show $ HS.size $ L._terminals t)
  pure t

maine :: IO ()
maine = do
  con <- readFile "../../test-machines/tmp/ep/EP_timeseq"
  let hs = initialization 5 con
  let x = runST $ test1 0.01 hs
  print x

findTerminals_ :: Double -> MLeaf s -> Seq (MLeaf s) -> STRef s [MLeaf s] -> ST s ()
findTerminals_ sig active q termsRef = do
  traceM . show $ S.length q
  unless (S.null q) $ do
    traceM "and again!"
    terms <- readSTRef termsRef
    hs <- readSTRef $ ML.histories active
    -- fs <- readSTRef $ ML.frequency active
    -- p  <- readSTRef $ ML.parent active
    -- tr <- readSTRef $ ML.terminalReference active
    -- he <- readSTRef $ ML.hasEdgeset active

    -- traceM $ "hs: " ++ (show $ fmap (view (Hist.bodyL . Hist.frequencyL)) $ HS.toList hs)
    -- traceM $ "fs: " ++ (show fs)
    -- traceM $ "he: " ++ (show he)

    isHomogeneous sig active >>= \case
      True -> do
        traceM "homogeneous!"
        let (nxtActive, next) = splitTerminals q
        findTerminals_ sig nxtActive next termsRef
      False -> do
        traceM "not homogeneous"
        let (nxtActive, next) = splitTerminals q
        cs' <- nextChilds active
        let cs'' = fmap snd cs'
        let ks'' = fmap fst cs'

        cs <- forM cs' $ \(e, x) -> findLoops sig x >>= (pure . (e,))
        forM_ cs $ uncurry (H.insert (ML.children active))   -- This leads to infinite recursion
        writeSTRef termsRef (cs'' <> delete active terms)
        findTerminals_ sig nxtActive (next <> S.fromList cs'') termsRef

