{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Data.MTree.Looping where

import CSSR.Prelude.Mutable
import Data.Alphabet
import CSSR.Probabilistic (Probabilistic)

import qualified CSSR.Probabilistic  as Prob
import qualified Data.Tree.Hist      as Hist
import qualified Data.Tree.Looping   as L

import qualified Data.HashSet              as HS
import qualified Data.HashMap.Strict       as HM
import qualified Data.Vector               as V
import qualified Data.Sequence             as S
import qualified Data.Vector.Mutable       as MV
import qualified Data.Vector.Generic       as GV
import qualified Data.HashTable.ST.Cuckoo  as C
import qualified Data.HashTable.Class      as H

-------------------------------------------------------------------------------
-- Mutable Looping Tree ADTs
-------------------------------------------------------------------------------
type HashTableSet s a = C.HashTable s a Bool

data MLeaf s = MLeaf
  -- for lack of a mutable hash set implementation
  { histories :: HashSet Hist.Leaf
  , frequency :: Vector Integer -- TODO => benchmark this vs. mutable version
  , children :: C.HashTable s Event (MLNode s)
  , parent :: STRef s (Maybe (MLeaf s))
  , terminalReference :: STRef s (Maybe (MLeaf s))
  , hasEdgeset :: STRef s Bool
  }

setTermRef :: MLeaf s -> Terminal s -> ST s ()
setTermRef leaf t = modifySTRef (terminalReference leaf) (const $ Just t)

getTermRef :: MLeaf s -> ST s (Maybe (Terminal s))
getTermRef = readSTRef . terminalReference

-- Don't do anything for now. Otherwise we loose a lot of purity.
addHistories :: MLeaf s -> HashSet Hist.Leaf -> ST s ()
addHistories leaf hs = pure ()

instance Hashable (MLeaf s) where
  hashWithSalt i a = hashWithSalt i (histories a, frequency a)

instance Probabilistic (MLeaf s) where
  frequency = frequency

type Loop s = MLeaf s
type Terminal s = MLeaf s

type MLNode s = Either (Loop s) (MLeaf s)

-- overlaps :: forall s . MLeaf s -> MLeaf s -> ST s Bool
-- overlaps l0 l1 = do
--   p0 <- readSTRef $ parent l0
--   p1 <- readSTRef $ parent l1
--   return $ mvecEq l0 l1 && p0 == p1
--   where
--     mvecEq = MV.overlaps `on` frequency

instance Eq (MLeaf s) where
  l0 == l1 = mvecEq l0 l1
    where
      mvecEq = (==) `on` frequency


data MTree s = MTree
  { terminals :: STRef s (HashSet (MLeaf s))
  , root :: MLeaf s
  }

addTerminal :: MTree s -> Terminal s -> ST s ()
addTerminal tree term = modifySTRef (terminals tree) (HS.insert term)


freezeTree :: forall s . MTree s -> ST s L.Tree
freezeTree tree = L.Tree
  <$> (readSTRef (terminals tree) >>= freezeTerms)
  <*> freeze (root tree)
  where
    freezeTerms :: HashSet (MLeaf s) -> ST s (HashSet L.Leaf)
    freezeTerms = fmap HS.fromList . mapM freeze . HS.toList


freeze :: forall s . MLeaf s -> ST s L.Leaf
freeze ml = do
  -- hs <- freezeHistories ml
  let f = frequency ml
  cs <- freezeDown =<< (H.toList . children $ ml)
  let cur = L.Leaf (Right (L.LeafBody (histories ml) f)) cs Nothing
  return $ withChilds cur (HM.map (withParent (Just cur)) cs)

  where
    withChilds :: L.Leaf -> HashMap Event L.Leaf -> L.Leaf
    withChilds (L.Leaf bod _ p) cs = L.Leaf bod cs p

    withParent :: Maybe L.Leaf -> L.Leaf -> L.Leaf
    withParent p (L.Leaf bod cs _) = L.Leaf bod cs p

    freezeHistories :: MLeaf s -> HashSet Hist.Leaf
    freezeHistories = histories

    freezeDown :: [(Event, MLNode s)] -> ST s (HashMap Event L.Leaf)
    freezeDown cs = do
      frz <- traverse icer cs
      return $ HM.fromList frz
      where
        icer :: (Event, MLNode s) -> ST s (Event, L.Leaf)
        icer (e, Left lp) = do
          let f = frequency lp
          -- hs <- (fmap.fmap) fst $ H.toList (histories lp)
          c <- freeze lp
          return (e, c)

        icer (e, Right lp) = do
          c <- freeze lp
          -- hs <- (fmap.fmap) fst $ H.toList (histories lp)
          return (e, c)


mkLeaf :: Maybe (MLeaf s) -> [Hist.Leaf] -> ST s (MLeaf s)
mkLeaf p hs =
  MLeaf
    <$> pure (HS.fromList hs) -- (fmap (, True) hs)
    <*> pure (foldr1 Prob.addFrequencies $ fmap (view (Hist.bodyL . Hist.frequencyL)) hs)
    <*> H.new
    <*> newSTRef p
    <*> newSTRef Nothing
    <*> newSTRef False

mkRoot :: Alphabet -> Hist.Leaf -> ST s (MLeaf s)
mkRoot (Alphabet vec _) hrt =
  MLeaf
    <$> pure (HS.fromList [hrt])
    <*> pure (V.replicate (V.length vec) 0)
    <*> H.new
    <*> newSTRef Nothing
    <*> newSTRef Nothing
    <*> newSTRef False

walk :: forall s . MLNode s -> Vector Event -> ST s (Maybe (MLNode s))
walk cur es
  | null es   = pure (Just cur)
  | otherwise =
    H.lookup (children (reify cur)) (V.head es)
    >>= \case
      Nothing -> return Nothing
      Just n  -> walk n (V.tail es)
  where
    reify :: MLNode s -> MLeaf s
    reify (Left  l) = l
    reify (Right l) = l


-------------------------------------------------------------------------------
-- Predicates for the construction of a looping tree

-- | === MarkEdgeSets
-- Psuedocode from paper:
--   INPUTS: looping node, all terminal nodes
--   COLLECT all terminal nodes that are not ancestors
--   IF exists collected terminal nodes with identical distributions
--   THEN
--     MARK looping node as an edge set
--     MARK found terminals as an edge set
--     // We will merge edgesets in Phase III.
--   ENDIF
markEdgeSets :: forall s . [MLeaf s] -> MLeaf s -> ST s ()
markEdgeSets terminals leaf = do
  edges <- edgeset =<< getAncestors leaf
  unless (null edges) $ do
    markEdge leaf
    forM_ edges markEdge

  where
    markEdge :: MLeaf s -> ST s ()
    markEdge lf = writeSTRef (hasEdgeset lf) True

    identicalDist :: MLeaf s -> ST s Bool
    identicalDist s = liftM2 (==) (distributionST leaf) (distributionST s)

    edgeset :: [MLeaf s] -> ST s [MLeaf s]
    edgeset ancestors = fmap nub . filterM identicalDist $ terminals \\ ancestors

    distributionST :: MLeaf s -> ST s (Vector Double)
    distributionST lf = pure $ Prob.freqToDist (frequency lf)


-- | === Homogeneity
-- Psuedocode from paper:
--   INPUTS: looping node, parse tree
--   COLLECT all next-step histories from looping node in parse tree
--   FOR each history in next-step histories
--     FOR each child in history's children
--       IF child's distribution ~/=  node's distribution
--       THEN RETURN false
--       ENDIF
--     ENDFOR
--   ENDFOR
--   RETURN TRUE
--
isHomogeneous :: forall s . Double -> MLeaf s -> ST s Bool
isHomogeneous sig ll = do
  let hs = fmap (view Hist.childrenL) ll'
  -- hs <- (fmap.fmap) (view Hist.childrenL . fst) ll'
  foldrM step True $ HS.fromList (hs >>= HM.elems)
  where
    ll' :: [Hist.Leaf]
    ll' = HS.toList (histories ll)

    step :: Hist.Leaf -> Bool -> ST s Bool
    step pc = \case
      False -> pure False
      True  -> pure $ Prob.matchesDists_ (frequency ll) (Prob.frequency pc) sig


-- | === Excisability
-- Psuedocode from paper:
--   INPUTS: looping node, looping tree
--   COLLECT all ancestors of the looping node from the looping tree, ordered by
--           increasing depth (depth 0, or "root node," first)
--   FOR each ancestor
--     IF ancestor's distribution == looping node's distribution
--     THEN
--       the node is excisable: create loop in the tree
--       ENDFOR (ie "break")
--     ELSE do nothing
--     ENDIF
--   ENDFOR
--
-- Examples:
-- >>> import CSSR.Algorithm.Phase1
-- >>> let ptree = initialization 2 short_ep
-- >>> :{
--
-- :}
-- >>> ptree
--
excisable :: forall s . Double -> MLeaf s -> ST s (Maybe (MLeaf s))
excisable sig ll = getAncestors ll >>= pure . go
  where
    go :: [MLeaf s] -> Maybe (MLeaf s)
    go     [] = Nothing
    go (a:as) =
      if Prob.matchesDists_ (frequency ll) (frequency a) sig
      then Just a
      else go as

-- |
-- Returns ancestors in order of how they should be processed
getAncestors :: MLeaf s -> ST s [MLeaf s]
getAncestors = go [] . Just
  where
    go :: [MLeaf s] -> Maybe (MLeaf s) -> ST s [MLeaf s]
    go ancestors = \case
      Nothing -> pure ancestors
      Just w  -> readSTRef (parent w) >>= go (w:ancestors)



