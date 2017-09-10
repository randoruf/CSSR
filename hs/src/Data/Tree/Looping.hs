{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Tree.Looping where

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H
import Data.Foldable

import CSSR.Prelude
import Data.Alphabet
import qualified Data.Tree.Hist as Hist

import CSSR.Probabilistic (Probabilistic)
import qualified CSSR.Probabilistic as Prob

data Leaf = Leaf
  { body      :: Either Leaf LeafBody
  , children  :: HashMap Event Leaf
  , parent    :: Maybe Leaf
  }

data LeafBody = LeafBody
  { histories :: HashSet Hist.Leaf
  , frequency :: Vector Integer
  } deriving (Show, Eq, Generic)

data Tree = Tree
  { _terminals :: HashSet Leaf
  , _root :: Leaf
  }

instance Eq Leaf where
  (Leaf b0 c0 _) == (Leaf b1 c1 _) = b0 == b1 && c0 == c1

instance Show Leaf where
  show (Leaf b c p) =
    case b of
      Right (LeafBody hs fs) -> strLeaf hs fs c
      Left (Leaf b' _ _)     -> case b' of
        Left _                 -> strErr c
        Right (LeafBody hs fs) -> strLoop hs fs c
    where
      strErr        c = "Leaf{Loop(<error>), " ++ show c ++"}"
      strLoop hs fs c = "Leaf{Loop(" ++ show hs ++ ", " ++ show fs ++ ", " ++ show c ++ ")}"
      strLeaf hs fs c = "Leaf{"      ++ show hs ++ ", " ++ show fs ++ ", " ++ show c ++  "}"

instance Probabilistic Leaf where
  frequency (Leaf b c p) =
    case b of
      Left (Leaf b' _ _) -> case b' of
          Left _                 -> error "should not exist"
          Right (LeafBody _ fs) -> fs
      Right (LeafBody _ fs) -> fs


instance Hashable LeafBody
instance Hashable Leaf where
  hashWithSalt salt (Leaf b _ _) = hashWithSalt salt b

--path :: forall f. Applicative f
--             => Vector Event
--             -> (LeafBody -> f LeafBody)
--             -> Leaf
--             -> f Leaf
--path events fn = go 0
--  where
--    go :: Int -> Leaf -> f Leaf
--    go dpth (Leaf body childs _) =
--      if dpth == V.length events - 1
--      then Leaf <$> fn body <*> pure childs <*> Nothing
--      else Leaf <$> fn body <*> nextChilds <*> Nothing
--
--      where
--        nextChilds :: f (HashMap Event Leaf)
--        nextChilds =
--          case HM.lookup c childs of
--            Just child -> HM.insert c <$> go (dpth + 1) child <*> pure childs
--            Nothing -> HM.insert c <$> buildNew dpth <*> pure childs
--          where
--            c :: Event
--            c = V.unsafeIndex events dpth
--
--
--        buildNew :: Int -> f Leaf
--        buildNew d
--          | d == V.length events - 1 = Leaf <$> mkBod events <*> pure mempty
--          | otherwise = Leaf <$> mkBod es <*> childs_
--          where
--            c :: Event
--            c = V.unsafeIndex events (d + 1)
--
--            es :: Vector Event
--            es = V.take (d + 1) events
--
--            mkBod :: Vector Event -> f LeafBody
--            mkBod es' = fn (LeafBody es' 0 mempty)
--
--            childs_ :: f (HashMap Char Leaf)
--            childs_ = HM.singleton c <$> buildNew (d + 1)



-------------------------------------------------------------------------------
-- Predicates for the consturction of a looping tree

-- | === isEdge
-- Psuedocode from paper:
--   INPUTS: looping node, looping tree
--   COLLECT all terminal nodes that are not ancestors
--   IF exists terminal nodes with identical distributions
--   THEN
--     mark looping node as an edge set
--     mark found terminals as an edge set
--     // We will merge edgesets in Phase III.
--   ENDIF
--
type EdgeGroup = (Vector Double, Vector Integer, HashSet Leaf)

groupEdges :: Double -> Tree -> HashSet EdgeGroup
groupEdges sig (Tree terms _) = HS.foldr part HS.empty terms
  where
    part :: Leaf -> HashSet EdgeGroup -> HashSet EdgeGroup
    part term groups =
      case foundEdge of
        Nothing -> HS.insert (termDist, termFreq, HS.singleton term) groups
        Just g  -> updateGroup g groups

      where
        termDist :: Vector Double
        termDist = Prob.distribution term

        termFreq :: Vector Integer
        termFreq = Prob.frequency term

        updateGroup :: EdgeGroup -> HashSet EdgeGroup -> HashSet EdgeGroup
        updateGroup g@(d, f, ts) groups =
          HS.insert (Prob.freqToDist summed, summed, HS.insert term ts) (HS.delete g groups)
          where
            summed :: Vector Integer
            summed = Prob.addFrequencies termFreq f

        foundEdge :: Maybe EdgeGroup
        foundEdge = HS.foldr matchEdges Nothing groups

        matchEdges :: EdgeGroup -> Maybe EdgeGroup -> Maybe EdgeGroup
        matchEdges _  g@(Just _) = g
        matchEdges g@(_, f, _) Nothing =
          if Prob.matchesDists_ (Prob.frequency term) f sig
          then Just g
          else Nothing


-- -- | === Homogeneity
-- -- Psuedocode from paper:
-- --   INPUTS: looping node, parse tree
-- --   COLLECT all next-step histories from looping node in parse tree
-- --   FOR each history in next-step histories
-- --     FOR each child in history's children
-- --       IF child's distribution ~/=  node's distribution
-- --       THEN RETURN false
-- --       ENDIF
-- --     ENDFOR
-- --   ENDFOR
-- --   RETURN TRUE
--
-- isHomogeneous :: Double -> Leaf -> Bool
-- isHomogeneous sig ll = foldr step True allPChilds
--   where
--     allPChilds :: HashSet Leaf
--     allPChilds = HS.fromList $
--       HS.toList (histories . body $ ll) >>= HM.elems . view Hist.children
--
--     step :: Leaf -> Bool -> Bool
--     step _  False = False
--     step pc _     = Prob.matches ll pc sig
--
-- -- | === Excisability
-- -- Psuedocode from paper:
-- --   INPUTS: looping node, looping tree
-- --   COLLECT all ancestors of the looping node from the looping tree, ordered by
-- --           increasing depth (depth 0, or "root node," first)
-- --   FOR each ancestor
-- --     IF ancestor's distribution == looping node's distribution
-- --     THEN
-- --       the node is excisable: create loop in the tree
-- --       ENDFOR (ie "break")
-- --     ELSE do nothing
-- --     ENDIF
-- --   ENDFOR
-- --
-- excisable :: Double -> Leaf -> Maybe Leaf
-- excisable sig ll = go (getAncestors ll)
--   where
--     go :: [Leaf] -> Maybe Leaf
--     go [] = Nothing
--     go (a:as)
--       | Prob.matches ll a sig = Just a
--       | otherwise = go as

excisable :: Double -> Leaf -> Maybe Leaf
excisable sig (Leaf (Left _) _ _) = Nothing
excisable sig ll@(Leaf (Right (LeafBody hs fs)) _ _) = go $ getAncestors ll
  where
    go :: [Leaf] -> Maybe Leaf
    go [] = Nothing
    go (a:as) = case body a of
      Left _ -> Nothing
      Right (LeafBody hs' fs') ->
        if Prob.matchesDists_ fs fs' sig
        then Just a
        else go as

-- | returns ancestors in order of how they should be processed
getAncestors :: Leaf -> [Leaf]
getAncestors ll = go (Just ll) []
  where
    go :: Maybe Leaf -> [Leaf] -> [Leaf]
    go  Nothing ancestors = ancestors
    go (Just w) ancestors = go (parent w) (w:ancestors)



