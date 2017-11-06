{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.MTree.Looping where

import CSSR.Prelude.Mutable

import qualified CSSR.Probabilistic  as Prob
import qualified Data.Tree.Conditional as Cond
import qualified Data.Tree.Looping   as L
import qualified Data.Tree.Internal  as I


import qualified Data.HashSet              as HS
import qualified Data.HashMap.Strict       as HM
import qualified Data.Vector               as V
import qualified Data.HashTable.ST.Cuckoo  as C
import qualified Data.HashTable.Class      as H

import Data.List.Set (ListSet(..))
import qualified Data.List.Set as S

type LLeaf = L.Leaf
type LTree = L.Tree

-------------------------------------------------------------------------------
-- Mutable Looping Tree ADTs
-------------------------------------------------------------------------------
type HashTableSet s a = C.HashTable s a Bool

data MLeaf s = MLeaf
  { path :: !(Vector Event)
  -- for lack of a mutable hash set implementation
  , histories :: !(STRef s (HashSet Cond.Leaf))
  , frequency :: !(STRef s (Vector Integer))
  , children :: !(C.HashTable s Event (MLNode s))
  , parent :: !(STRef s (Maybe (MLeaf s)))
  , terminalReference :: !(STRef s (Maybe (MLeaf s)))
  , hasEdgeset :: !(STRef s Bool)
  }

instance Eq (MLeaf s) where
  a == b
    = path a == path b
    && histories a == histories b
    && frequency a == frequency b
    && parent a == parent b
    && terminalReference a == terminalReference b
    && hasEdgeset a == hasEdgeset b


setTermRef :: MLeaf s -> MTerminal s -> ST s ()
setTermRef leaf t = modifySTRef (terminalReference leaf) (const $ Just t)


getTermRef :: MLeaf s -> ST s (Maybe (MTerminal s))
getTermRef = readSTRef . terminalReference


childHistories :: MLeaf s -> ST s [Cond.Leaf]
childHistories
  = fmap (foldMap (HM.elems . Cond.children) . HS.toList)
  . readSTRef
  . histories


addHistories :: MLeaf s -> HashSet Cond.Leaf -> ST s ()
addHistories leaf hs = do
  modifySTRef (frequency leaf) addFreqs
  modifySTRef (histories leaf) (HS.union hs)
  where
    addFreqs :: Vector Integer -> Vector Integer
    addFreqs leafFs = foldr ((<+>) . hfreq) leafFs (HS.toList hs)

    hfreq :: Cond.Leaf -> Vector Integer
    hfreq = view (Cond.bodyL . Cond.frequencyL)

    -- Vector addition which sanity-checks the impossible case
    (<+>) :: Vector Integer -> Vector Integer -> Vector Integer
    a <+> b =
      if V.length a /= V.length b
      then impossible "distributions will all be of length size(Alphabet)"
      else V.zipWith (+) a b


toNodeList :: MLeaf s -> ST s [MLNode s]
toNodeList l = go [] [Left l]
 where
  go :: [MLNode s] -> [MLNode s] -> ST s [MLNode s]
  go ret [] = pure ret
  go ret ps = do
    cs <- foldMapM (H.toList . children) (rights ps)
    go (ret <> ps) (map snd cs)

  foldMapM :: (Monoid m, Foldable t, Monad st) => (a -> st m) -> t a -> st m
  foldMapM f = foldrM (\a m -> (<> m) <$> f a) mempty

type MLoop s = MLeaf s
type MTerminal s = MLeaf s
type MLNode s = Either (MLoop s) (MLeaf s)

data MTree s = MTree
  { terminals :: STRef s (ListSet (MTerminal s))
  , root :: MLeaf s
  }

addTerminal :: MTree s -> MTerminal s -> ST s ()
addTerminal tree term = modifySTRef (terminals tree) (S.insert term)

freezeTree :: forall s . MTree s -> ST s LTree
freezeTree tree = do
  rt <- freeze (root tree)
  ts <- readSTRef (terminals tree)
  pure $ L.Tree (findTerms (fmap path ts) rt) rt
  where
    findTerms :: ListSet (Vector Event) -> LLeaf -> HashSet LLeaf
    findTerms (unListSet->ts) rt =
      if length ts /= length checklist
      then impossible "all terminals must be found in the frozen tree"
      else HS.fromList checklist
     where
      checklist :: [LLeaf]
      checklist = mapMaybe (\t -> rt ^? ix t) ts

freeze :: MLeaf s -> ST s LLeaf
freeze ml = do
  rt <- initOne ml
  go rt [(path ml, children ml)]
 where
  go :: LLeaf -> [(Vector Event, HashTable s Event (MLNode s))] -> ST s LLeaf
  go rt [] = pure rt
  go rt ((p,cs):nxt) = do
    childs <- freezeChildren rt p =<< H.toList cs
    let
      pl   = fromMaybe (impossible "parent must exist") (rt ^? ix p)
      rt'  = rt & ix p . L.lchildrenL .~ childs
      rt'' = foldr (setParent pl . V.snoc p) rt' (HM.keys childs)

    append <- mapMaybe (uncurry (queueNext p)) <$> H.toList cs
    go rt' (nxt <> append)

  setParent :: LLeaf -> Vector Event -> LLeaf -> LLeaf
  setParent p cpath rt = rt & ix cpath . L.parentL .~ Just p

  queueNext
    :: Vector Event
    -> Event
    -> MLNode s
    -> Maybe (Vector Event, HashTable s Event (MLNode s))
  queueNext p e = either (const Nothing) (Just . (p `V.snoc` e,) . children)

freezeChildren
  :: LLeaf
  -> Vector Event
  -> [(Event, MLNode s)]
  -> ST s (HashMap Event LLeaf)
freezeChildren rt p cs = HM.fromList . catMaybes <$> mapM icer cs
 where
  icer :: (Event, MLNode s) -> ST s (Maybe (Event, LLeaf))
  icer (e, c) = either (mkLoop e) (mkLeaf e) c

  mkLoop, mkLeaf :: Event -> MLeaf s -> ST s (Maybe (Event, LLeaf))
  mkLeaf e nxt = Just . (e,) <$> initOne nxt
  mkLoop e c = pure . Just $ (e, L.Leaf (p `V.snoc` e) (Left lp) Nothing)
   where
    lp :: LLeaf
    lp = fromMaybe (impossible "bad loop!") (L.navigate rt (path c))

initOne :: MLeaf s -> ST s LLeaf
initOne ml = do
  f  <- readSTRef (frequency ml)
  hs <- readSTRef (histories ml)
  pure $ L.Leaf (path ml) (Right $ L.LeafBody hs f mempty) Nothing

-- ========================================================================= --


mkLeaf :: Maybe (MLeaf s) -> Event -> [Cond.Leaf] -> ST s (MLeaf s)
mkLeaf p e hs = MLeaf lpath
  <$> newSTRef (HS.fromList hs)
  <*> newSTRef (foldr1 Prob.addFrequencies $ fmap (view (Cond.bodyL . Cond.frequencyL)) hs)
  <*> H.new
  <*> newSTRef p
  <*> newSTRef Nothing
  <*> newSTRef False
 where
  lpath :: Vector Event
  lpath = maybe mempty path p `V.snoc` e


mkRoot :: Cond.Leaf -> ST s (MLeaf s)
mkRoot hrt = MLeaf mempty
  <$> newSTRef (HS.fromList [hrt])
  <*> newSTRef (view (Cond.bodyL . Cond.frequencyL) hrt)
  <*> H.new
  <*> newSTRef Nothing
  <*> newSTRef Nothing
  <*> newSTRef False


navigateM :: forall s . MLNode s -> Vector Event -> ST s (Maybe (MLNode s))
navigateM = I.navigateM (H.lookup . children . reify)
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
    distributionST lf = Prob.freqToDist <$> readSTRef (frequency lf)

-- ========================================================================= --

isHomogeneous :: forall s . Double -> MLeaf s -> ST s Bool
isHomogeneous sig ll = do
  pdist <- readSTRef (frequency ll)
  I.isHomogeneousM childDists sig (ll, pdist)
  where
    childDists :: MLeaf s -> ST s [Vector Integer]
    childDists = (fmap.fmap) Prob.frequency . childHistories

excisable :: Double -> MLeaf s -> ST s (Maybe (MLeaf s))
excisable = I.excisableM (readSTRef . parent) (readSTRef . frequency)

getAncestors :: MLeaf s -> ST s [MLeaf s]
getAncestors = I.getAncestorsM (readSTRef . parent)

