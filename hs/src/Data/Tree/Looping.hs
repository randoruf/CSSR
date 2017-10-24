{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
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
import qualified Data.Text as T
import Data.Foldable

import CSSR.Prelude
import Data.Alphabet
import qualified Data.Tree.Conditional as Cond
import qualified Data.Tree.Internal as I


import CSSR.Probabilistic (Probabilistic, TestResult(..))
import qualified CSSR.Probabilistic as Prob


data Leaf = Leaf
  { body      :: Either LeafRep LeafBody
  , children  :: HashMap Event Leaf
  , parent    :: Maybe Leaf
  } deriving (Generic, NFData)

bodyL :: Lens' Leaf (Either LeafRep LeafBody)
bodyL = lens body $ \l b -> l { body = b }

childrenL :: Lens' Leaf (HashMap Event Leaf)
childrenL = lens children $ \l x -> l { children = x }

newtype LeafRep = LeafRep
  { path :: Vector Event
  } deriving (Show, Eq, Generic, NFData)

pathL :: Lens' LeafRep (Vector Event)
pathL = lens path $ \b f -> b { path = f }

data LeafBody = LeafBody
  { histories :: HashSet Cond.Leaf
  , frequency :: Vector Integer
  } deriving (Eq, Generic, NFData)

historiesL :: Lens' LeafBody (HashSet Cond.Leaf)
historiesL = lens histories $ \b f -> b { histories = f }

frequencyL :: Lens' LeafBody (Vector Integer)
frequencyL = lens frequency $ \b f -> b { frequency = f }

data Tree = Tree
  { terminals :: HashSet Leaf
  , root :: Leaf
  } deriving (Eq, Generic, NFData)

terminalsL :: Lens' Tree (HashSet Leaf)
terminalsL = lens terminals $ \b f -> b { terminals = f }

rootL :: Lens' Tree Leaf
rootL = lens root $ \b f -> b { root = f }

instance Eq Leaf where
  (Leaf b0 c0 _) == (Leaf b1 c1 _) = b0 == b1 && c0 == c1

instance Show Tree where
  show (Tree ts rt)
    = unlines . mconcat $
      [ [ "Looping.Tree"
        , "terminals:"
        ]
      , (("\t"<>) . showLeaf False) <$> HS.toList ts
      , [ "root:"
        , show rt
        ]
      ]

instance Show Leaf where
  show l = I.showLeaf toConds toFreqs toChilds "Looping" (toRepr l) l
    where
      toConds :: Leaf -> (Bool, [Vector Event])
      toConds l =
        case body l of
          Left (LeafRep p) -> (True, [p])
          Right (LeafBody hs _) -> (False, view Cond.lobsL <$> HS.toList hs)

      toFreqs :: Leaf -> [Vector Integer]
      toFreqs = either (const []) ((:[]) . view frequencyL) . body

      toChilds :: Leaf -> [(Event, Leaf)]
      toChilds = HM.toList . view childrenL

      toRepr :: Leaf -> Vector Event
      toRepr = either path (fromMaybe mempty . head . toHObs) . body
        where
          toHObs :: LeafBody -> [Vector Event]
          toHObs = fmap (view Cond.lobsL) . HS.toList . histories


  -- show = go 1 " "
  --  where
      indent :: Int -> String
      indent d = replicate (5 * d) ' '

      showLeaf :: Int -> Event -> Either LeafRep LeafBody -> String
      showLeaf d e b = "\n" ++ indent d ++ show e ++"->LLeaf{" ++ show b

      go :: Int -> Event -> Leaf -> String
      go d e (Leaf b cs _)
        | null cs = showLeaf d e b ++ ", no children}"
        | otherwise = showLeaf d e b ++ "}\n"
                      ++ indent (d + 1) ++ "children:"
                      ++ (intercalate "\n" . map (uncurry (go (d+1))) . HM.toList $ cs)

instance Show LeafBody where
  show (LeafBody h f) =
    "hists: " ++ Cond.showAllObs (HS.toList h) ++ ", freq: " ++ show f


showLeaf :: Bool -> Leaf -> String
showLeaf full (Leaf b c p) =
  case b of
    Right (LeafBody hs fs) -> strLeaf hs fs c
    Left  (LeafRep v) -> strLoop v
  where
    showHS :: HashSet Cond.Leaf -> String
    showHS = show . fmap (show . Cond.obs . Cond.body) . HS.toList

    strLoop p       = "Leaf{Loop(" ++ show p ++ ")}"
    strLeaf hs fs c = "Leaf{"      ++ showHS hs ++ ", " ++ show (Prob.freqToDist fs) ++ strChildren c ++  "}"
    strChildren c =
      if full
      then ", " ++ show c
      else ""

instance Hashable LeafBody
instance Hashable LeafRep
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

