{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Tree.Looping where

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V


import CSSR.Prelude

import qualified Data.Tree.Conditional as Cond (Leaf, lobsL, showAllObs)
import qualified Data.Tree.Internal as I
import qualified CSSR.Probabilistic as Prob (freqToDist)

type Terminal = Leaf

data Leaf = Leaf
  { path   :: Vector Event
  , body   :: Either Leaf LeafBody
  , parent :: Maybe Leaf
  } deriving (Generic, NFData)


instance Show Leaf where
  show l = I.showLeaf toConds toFreqs toChilds "Looping" (path l) l
   where
    toConds :: Leaf -> (Bool, [Vector Event])
    toConds l = case body l of
      Left  lp -> (True, [path lp])
      Right bd -> (False, view Cond.lobsL <$> toList (histories bd))

    toFreqs :: Leaf -> [Vector Integer]
    toFreqs = either (const []) ((:[]) . view frequencyL) . body

    toChilds :: Leaf -> [(Event, Leaf)]
    toChilds l = either (const []) (HM.toList . view childrenL) (body l)

instance Hashable Leaf where
  hashWithSalt salt l = hashWithSalt salt (pToHashable l, mapMaybe pToHashable ps)
   where
    ps :: [Leaf]
    ps = I.getAncestors parent l

    pToHashable :: Leaf -> Maybe (HashSet Cond.Leaf, Vector Integer)
    pToHashable = fmap (histories &&& frequency) . preview (bodyL . _Right)

instance Eq Leaf where
  l0 == l1 =
    -- path is our primary identifier
    path l0 == path l1

    -- compare NON-LOOPING components of bodies
    && preview (bodyL . _Right) l0 == preview (bodyL . _Right) l1

    -- probably not required, but if we wind up comparing loops, go up the chain
    -- && parent l0 == parent l1


bodyL :: Lens' Leaf (Either Leaf LeafBody)
bodyL = lens body $ \l b -> l { body = b }

pathL :: Lens' Leaf (Vector Event)
pathL = lens path $ \b f -> b { path = f }

parentL :: Lens' Leaf (Maybe Leaf)
parentL = lens parent $ \b f -> b { parent = f }

-- TODO: decide if LeafRep is a better implementation
data LeafRep = LeafRep deriving (Show, Eq, Generic, NFData)

instance Hashable LeafRep

data LeafBody = LeafBody
  { histories :: HashSet Cond.Leaf
  , frequency :: Vector Integer
  , children  :: HashMap Event Leaf
  } deriving (Eq, Generic, NFData)

instance Hashable LeafBody

instance Show LeafBody where
  show (LeafBody h f _) = "hists: " ++ Cond.showAllObs (toList h) ++ ", freq: " ++ show f

historiesL :: Lens' LeafBody (HashSet Cond.Leaf)
historiesL = lens histories $ \b f -> b { histories = f }

frequencyL :: Lens' LeafBody (Vector Integer)
frequencyL = lens frequency $ \b f -> b { frequency = f }

childrenL :: Lens' LeafBody (HashMap Event Leaf)
childrenL = lens children $ \l x -> l { children = x }

lchildrenL :: ASetter' Leaf (HashMap Event Leaf)
lchildrenL = bodyL . _Right . childrenL

data Tree = Tree
  { terminals :: HashSet Leaf
  , root :: Leaf
  } deriving (Eq, Generic, NFData)


instance Show Tree where
  show (Tree ts rt)
    = unlines . mconcat $
      [ [ "Looping.Tree"
        , "terminals:"
        ]
      , (("\t"<>) . showTerm) <$> toList ts
      , [ "root:"
        , show rt
        ]
      ]
   where
    showTerm :: Leaf -> String
    showTerm l =
      case body l of
        Right b -> strLeaf l (toList (histories b)) (frequency b)
        Left lp -> "Leaf{Loop(" ++ show (path lp) ++ ")}"

    strLeaf :: Leaf -> [Cond.Leaf] -> Vector Integer -> String
    strLeaf l hs fs = intercalate ", "
      [ "Leaf{" ++ show (path l)
      , Cond.showAllObs hs
      , show (Prob.freqToDist fs) ++ "}"
      ]


terminalsL :: Lens' Tree (HashSet Leaf)
terminalsL = lens terminals $ \b f -> b { terminals = f }

rootL :: Lens' Tree Leaf
rootL = lens root $ \b f -> b { root = f }

navigate :: Leaf -> Vector Event -> Maybe Leaf
navigate l = I.navigate go l
 where
  go :: Leaf -> HashMap Event Leaf
  go (Leaf _ (Right bd) _) = children bd
  go (Leaf _ (Left lp) _) = view (bodyL . _Right . childrenL) lp

navigate' :: Leaf -> Vector Event -> Maybe Leaf
navigate' l = I.navigate' (const True) go l
 where
  go :: Leaf -> HashMap Event Leaf
  go = either (view (bodyL . _Right . childrenL)) children . body

ancestors :: Leaf -> [Leaf]
ancestors = I.getAncestors parent

-------------------------------------------------------------------------------
-- Lenses for Looping Tree
-------------------------------------------------------------------------------
type instance Index Leaf = Vector Event
type instance IxValue Leaf = Leaf

instance Ixed Leaf where
  ix :: Vector Event -> Traversal' Leaf (IxValue Leaf)
  ix histories = go (V.length histories - 1)
    where
      go dpth f p@(Leaf pth bod mprnt)
        | dpth < 0  = f p
        | otherwise = either
            (go dpth f)
            (\b -> case HM.lookup c (children b) of
              Nothing -> pure p
              Just child -> goAgain <$> go (dpth-1) f child
                where
                  goAgain :: Leaf -> Leaf
                  goAgain child' = Leaf pth (Right $ b { children = HM.insert c child' (children b)} ) mprnt
            ) bod
        where
          c :: Event
          c = histories ! dpth

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
groupEdges = undefined -- HS.foldr part HS.empty terms
{-
groupEdges sig (Tree terms _) = undefined -- HS.foldr part HS.empty terms
  where
    part :: Leaf -> HashSet EdgeGroup -> HashSet EdgeGroup
    part term groups =
      case foundEdge of
        Nothing -> HS.insert (termDist, termFreq, HS.singleton term) groups
        Just g  -> updateGroup g groups

      where
        termDist :: Vector Double
        termDist = undefined -- Prob.distribution term

        termFreq :: Vector Integer
        termFreq = undefined -- Prob.frequency term

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
        matchEdges g@(_, f, _) Nothing = undefined
          -- if Prob.matchesDists_ (Prob.frequency term) f sig
          -- then Just g
          -- else Nothing

excisable :: Double -> Leaf -> Maybe Leaf
excisable s l = join $ I.excisableM (Just . parent) (preview (bodyL . _Right . frequencyL)) s l
-}

