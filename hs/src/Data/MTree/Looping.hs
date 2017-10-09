{-# LANGUAGE TupleSections #-}
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
import qualified Data.Tree.Internal  as I

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
  -- let cur = L.Leaf (Right (L.LeafBody hs f)) cs Nothing
  -- return $ withChilds cur (HM.map (withParent (Just cur)) cs)
  return $ L.Leaf (Right (L.LeafBody hs f)) HM.empty Nothing -- cur (HM.map (withParent (Just cur)) cs)

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
          let f = frequency lp
          -- hs <- (fmap.fmap) fst $ H.toList (histories lp)
          -- c <- freeze lp
          return $ Just (e, L.Leaf (Right $ L.LeafBody HS.empty (V.fromList [])) mempty Nothing)

        icer (e, Right lp) =
          Just . (e,) <$> freeze lp

-- data Leaf = Leaf
--   { body      :: Either Leaf LeafBody
--   , children  :: HashMap Event Leaf
--   , parent    :: Maybe Leaf
--   } deriving (Generic, NFData)
--
-- data LeafBody = LeafBody
--   { histories :: HashSet Hist.Leaf
--   , frequency :: Vector Integer
--   } deriving (Show, Eq, Generic, NFData)



mkLeaf :: Maybe (MLeaf s) -> [Hist.Leaf] -> ST s (MLeaf s)
mkLeaf p hs = do
  a0 <- newSTRef (HS.fromList hs) -- (fmap (, True) hs)
  a1 <- newSTRef (foldr1 Prob.addFrequencies $ fmap (view (Hist.bodyL . Hist.frequencyL)) hs)
  a2 <- H.new
  a3 <- newSTRef p
  a4 <- newSTRef Nothing
  a5 <- newSTRef False
  pure $ MLeaf a0 a1 a2 a3 a4 a5


mkRoot :: Alphabet -> Hist.Leaf -> ST s (MLeaf s)
mkRoot (Alphabet vec _) hrt =
  MLeaf
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
  pdist <- readSTRef $ frequency ll
  childs <- childHistories ll
  hists <- l2hists ll
  pure $ all (cMatchesP pdist . Prob.frequency) childs
  where
    l2hists :: MLeaf s -> ST s [Vector Integer]
    l2hists lf = do
      hs <- fmap toList . readSTRef . histories $ lf
      pure $ fmap (view (Hist.bodyL . Hist.frequencyL)) hs

    cMatchesP :: Vector Integer -> Vector Integer -> Bool
    cMatchesP pdist cdist = Prob.matchesDists_ pdist cdist sig


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
excisable sig ll = getAncestors ll >>= go
  where
    go :: [MLeaf s] -> ST s (Maybe (MLeaf s))
    go     [] = pure Nothing
    go (a:as) = do
      af <- readSTRef (frequency a)
      llf <- readSTRef (frequency ll)
      if Prob.matchesDists_ llf af sig
      then pure $ Just a
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



