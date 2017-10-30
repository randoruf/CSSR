{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.MTree.Looping where

import CSSR.Prelude.Mutable
import Data.Alphabet

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


-------------------------------------------------------------------------------
-- Mutable Looping Tree ADTs
-------------------------------------------------------------------------------
type HashTableSet s a = C.HashTable s a Bool

data MLeaf s = MLeaf
  -- for lack of a mutable hash set implementation
  { histories :: STRef s (HashSet Cond.Leaf)
  , frequency :: STRef s (Vector Integer)
  , children :: C.HashTable s Event (MLNode s)
  , parent :: STRef s (Maybe (MLeaf s))
  , terminalReference :: STRef s (Maybe (MLeaf s))
  , hasEdgeset :: STRef s Bool
  }

instance Eq (MLeaf s) where
  a == b
    = histories a == histories b
    && frequency a == frequency b
    && parent a == parent b
    && terminalReference a == terminalReference b
    && hasEdgeset a == hasEdgeset b

setTermRef :: MLeaf s -> Terminal s -> ST s ()
setTermRef leaf t = modifySTRef (terminalReference leaf) (const $ Just t)


getTermRef :: MLeaf s -> ST s (Maybe (Terminal s))
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


type Loop s = MLeaf s
type Terminal s = MLeaf s
type MLNode s = Either (Loop s) (MLeaf s)

data MTree s = MTree
  { terminals :: STRef s (ListSet (Terminal s))
  , root :: MLeaf s
  }

addTerminal :: MTree s -> Terminal s -> ST s ()
addTerminal tree term = modifySTRef (terminals tree) (S.insert term)

freezeTree :: forall s . MTree s -> ST s L.Tree
freezeTree tree = L.Tree
  <$> (readSTRef (terminals tree) >>= freezeTerms)
  <*> freeze (root tree)
  where
    freezeTerms :: ListSet (MLeaf s) -> ST s (HashSet L.Leaf)
    freezeTerms = fmap HS.fromList . mapM freeze . S.toList


freeze :: forall s . MLeaf s -> ST s L.Leaf
freeze ml = do
  f <- readSTRef (frequency ml)
  cs <- freezeDown =<< (H.toList . children $ ml)
  hs <- readSTRef (histories ml)

  let cur = L.Leaf (Right (L.LeafBody hs f)) cs Nothing
  return $ withChilds cur (HM.map (withParent (Just cur)) cs)

  where
    withChilds :: L.Leaf -> HashMap Event L.Leaf -> L.Leaf
    withChilds (L.Leaf bod _ p) cs = L.Leaf bod cs p

    withParent :: Maybe L.Leaf -> L.Leaf -> L.Leaf
    withParent p (L.Leaf bod cs _) = L.Leaf bod cs p

    freezeDown :: [(Event, MLNode s)] -> ST s (HashMap Event L.Leaf)
    freezeDown cs = HM.fromList . catMaybes <$> mapM icer cs
      where
        icer :: (Event, MLNode s) -> ST s (Maybe (Event, L.Leaf))
        icer (e, Left lp) = do
          hs <- minimum . fmap (Cond.obs . Cond.body) . HS.toList <$> readSTRef (histories lp)
          case hs of
            Nothing -> impossible "all leaves have at least one history (TODO: move to NonEmptySet)"
            Just h  -> return $ Just (e, L.Leaf (Left $ L.LeafRep h) mempty Nothing)

        icer (e, Right lp) = Just . (e,) <$> freeze lp


mkLeaf :: Maybe (MLeaf s) -> [Cond.Leaf] -> ST s (MLeaf s)
mkLeaf p hs = MLeaf
  <$> newSTRef (HS.fromList hs)
  <*> newSTRef (foldr1 Prob.addFrequencies $ fmap (view (Cond.bodyL . Cond.frequencyL)) hs)
  <*> H.new
  <*> newSTRef p
  <*> newSTRef Nothing
  <*> newSTRef False


mkRoot :: Alphabet -> Cond.Leaf -> ST s (MLeaf s)
mkRoot _ hrt = MLeaf
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

excisable :: forall s . Double -> MLeaf s -> ST s (Maybe (MLeaf s))
excisable sig ll = I.excisableM (readSTRef . parent) (readSTRef . frequency) sig ll

getAncestors :: MLeaf s -> ST s [MLeaf s]
getAncestors = I.getAncestorsM (readSTRef . parent)

