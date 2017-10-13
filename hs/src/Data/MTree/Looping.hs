{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Data.MTree.Looping where

import CSSR.Prelude.Mutable
import Data.Alphabet
import CSSR.Probabilistic (Probabilistic, TestResult(..))

import qualified CSSR.Probabilistic  as Prob
import qualified Data.Tree.Hist      as Hist
import qualified Data.Tree.Looping   as L
import qualified Data.Tree.Internal  as I
import qualified Data.Text as T


import qualified Data.HashSet              as HS
import qualified Data.HashMap.Strict       as HM
import qualified Data.Vector               as V
import qualified Data.Sequence             as S
import qualified Data.Vector.Mutable       as MV
import qualified Data.Vector.Generic       as GV
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
  { histories :: STRef s (HashSet Hist.Leaf)
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


childHistories :: MLeaf s -> ST s [Hist.Leaf]
childHistories
  = fmap (foldMap (HM.elems . Hist.children) . HS.toList)
  . readSTRef
  . histories


-- Don't do anything for now. Otherwise we loose a lot of purity.
addHistories :: MLeaf s -> HashSet Hist.Leaf -> ST s ()
addHistories leaf hs = do
  modifySTRef (frequency leaf) addFreqs
  modifySTRef (histories leaf) (HS.union hs)
  where
    addFreqs :: Vector Integer -> Vector Integer
    addFreqs leafFs = foldr ((<+>) . hfreq) leafFs (HS.toList hs)

    hfreq :: Hist.Leaf -> Vector Integer
    hfreq = view (Hist.bodyL . Hist.frequencyL)

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

    histObs :: Hist.Leaf -> Vector Event
    histObs = Hist.obs . Hist.body

    freezeDown :: [(Event, MLNode s)] -> ST s (HashMap Event L.Leaf)
    freezeDown cs = HM.fromList . catMaybes <$> mapM icer cs
      where
        icer :: (Event, MLNode s) -> ST s (Maybe (Event, L.Leaf))
        icer (e, Left lp) = do
          traceM "icer left"
          let f = frequency lp
          hs <- head . sort . fmap (Hist.obs . Hist.body) . HS.toList <$> readSTRef (histories lp)
          case hs of
            Nothing -> impossible "all leaves have at least one history (TODO: move to NonEmpty)"
            Just h  -> return $ Just (e, L.Leaf (Left $ L.LeafRep h) mempty Nothing)

        icer (e, Right lp) = do
          traceM "icer right"
          Just . (e,) <$> freeze lp


mkLeaf :: Maybe (MLeaf s) -> [Hist.Leaf] -> ST s (MLeaf s)
mkLeaf p hs = MLeaf
  <$> newSTRef (HS.fromList hs)
  <*> newSTRef (foldr1 Prob.addFrequencies $ fmap (view (Hist.bodyL . Hist.frequencyL)) hs)
  <*> H.new
  <*> newSTRef p
  <*> newSTRef Nothing
  <*> newSTRef False


mkRoot :: Alphabet -> Hist.Leaf -> ST s (MLeaf s)
mkRoot (Alphabet vec _) hrt = MLeaf
  <$> newSTRef (HS.fromList [hrt])
  <*> newSTRef (view (Hist.bodyL . Hist.frequencyL) hrt)
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


-- Note that walk and navigate go in reverse walking order >.<
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
excisable sig ll = do
  hs <- readSTRef (histories ll)
  traceM $ "current: " <> showHists (HS.toList hs)
  as <- getAncestors ll
  ahs <- mapM (readSTRef . histories) as
  traceM $ "ancestors: " <> show (map (showHists . HS.toList) ahs)
  I.excisableM (readSTRef . parent) (readSTRef . frequency) sig ll

getAncestors :: MLeaf s -> ST s [MLeaf s]
getAncestors = I.getAncestorsM (readSTRef . parent)

showHists :: [Hist.Leaf] -> String
showHists = show . fmap (T.concat . V.toList . Hist.obs . Hist.body)

showHDists :: HashSet Hist.Leaf -> String
showHDists = show . fmap (intercalate "," . V.toList . fmap f'4 . Prob.freqToDist . Hist.frequency . Hist.body) . HS.toList



