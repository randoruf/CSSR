{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Tree.Looping where

import qualified Data.HashMap.Strict as HM (toList, lookup)

import CSSR.Prelude

import qualified Data.Tree.Conditional as Cond (Leaf, lobsL, showAllObs)
import qualified Data.Tree.Internal as I
import qualified CSSR.Probabilistic as Prob (freqToDist)

type Terminal = Leaf

data Leaf = Leaf
  { path   :: Vector Event
  , body   :: Either (Either (Vector Event) {-super hack to freeze-} Leaf) LeafBody
  , parent :: Maybe Leaf
  } deriving (Generic, NFData)


defLeaf :: Vector Event -> LeafBody -> Leaf
defLeaf a b = Leaf a (Right b) Nothing

mkLeaf :: Vector Event -> LeafBody -> Maybe Leaf -> Leaf
mkLeaf a b c = Leaf a (Right b) c

defLoop :: Vector Event -> Leaf -> Leaf
defLoop a b = Leaf a (Left (Right b)) Nothing

defUninitializedLoop :: Vector Event -> Vector Event -> Leaf
defUninitializedLoop a b = Leaf a (Left (Left b)) Nothing


instance Show Leaf where
  show l = I.showLeaf toConds toFreqs toChilds "Looping" (path l) l
    where
      toConds :: Leaf -> (Bool, [Vector Event])
      toConds l =
        case body l of
          Left  lp -> (True, [either (const mempty) path lp])
          Right bd -> (False, view Cond.lobsL <$> toList (histories bd))

      toFreqs :: Leaf -> [Vector Integer]
      toFreqs = either (const []) ((:[]) . view frequencyL) . body

      toChilds :: Leaf -> [(Event, Leaf)]
      toChilds l = either (const []) (HM.toList . view childrenL) (body l)

instance Hashable Leaf where
  hashWithSalt salt l = hashWithSalt salt (mapMaybe pToHashable ps)
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
    && parent l0 == parent l1


bodyL :: Lens' Leaf (Either (Either (Vector Event) Leaf) LeafBody)
bodyL = lens body $ \l b -> l { body = b }

pathL :: Lens' Leaf (Vector Event)
pathL = lens path $ \b f -> b { path = f }

parentL :: Lens' Leaf (Maybe Leaf)
parentL = lens parent $ \b f -> b { parent = f }

data LeafRep = LeafRep deriving (Show, Eq, Generic, NFData)

instance Hashable LeafRep

data LeafBody = LeafBody
  { histories :: HashSet Cond.Leaf
  , frequency :: Vector Integer
  , children  :: HashMap Event Leaf
  } deriving (Eq, Generic, NFData)

instance Hashable LeafBody

instance Show LeafBody where
  show (LeafBody h f c) =
    "hists: " ++ Cond.showAllObs (toList h) ++ ", freq: " ++ show f

historiesL :: Lens' LeafBody (HashSet Cond.Leaf)
historiesL = lens histories $ \b f -> b { histories = f }

frequencyL :: Lens' LeafBody (Vector Integer)
frequencyL = lens frequency $ \b f -> b { frequency = f }

childrenL :: Lens' LeafBody (HashMap Event Leaf)
childrenL = lens children $ \l x -> l { children = x }


data Tree = Tree
  { terminals :: HashSet Leaf
  , root :: Leaf
  } deriving (Eq, Generic, NFData)

terminalsL :: Lens' Tree (HashSet Leaf)
terminalsL = lens terminals $ \b f -> b { terminals = f }

rootL :: Lens' Tree Leaf
rootL = lens root $ \b f -> b { root = f }

ancestors :: Leaf -> [Leaf]
ancestors l = trace (show (path l) ++ " ancestors: " ++ show (fmap path p)) p
  where p = I.getAncestors parent l


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
        Left lp -> strLoop (either undefined identity lp)

    strLoop :: Leaf -> String
    strLoop p     = "Leaf{Loop(" ++ show (path p) ++ ")}"

    strLeaf :: Leaf -> [Cond.Leaf] -> Vector Integer -> String
    strLeaf l hs fs = intercalate ", "
      [ "Leaf{" ++ show (path l)
      , Cond.showAllObs hs
      , show (Prob.freqToDist fs) ++ "}"
      ]

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

