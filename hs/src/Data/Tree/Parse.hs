{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.Tree.Parse where

import Data.List
import qualified Data.Text            as T
import qualified Data.HashMap.Strict  as HM
import qualified Data.HashSet         as HS
import qualified Data.Vector          as V
import qualified Data.Tree.Internal   as I

import CSSR.Prelude

-- $ setup
-- >>> :set -XTypeFamilies
-- >>> import qualified Data.Vector as V

-------------------------------------------------------------------------------
-- Parse Tree ADTs
-------------------------------------------------------------------------------
data Tree = Tree
  { depth :: Int
  , root :: Leaf
  } deriving (Eq, Generic)

rootL :: Lens' Tree Leaf
rootL = lens root $ \lf a -> lf { root = a }

depthL :: Lens' Tree Int
depthL = lens depth $ \lf a -> lf { depth = a }

instance Show Tree where
  show (Tree d r) = "ParseTree{depth: " ++ show d ++ "}\n  root:" ++ show r

-------------------------------------------------------------------------------
-- Leaf ADT

data Leaf = Leaf
  { body :: LeafBody
  , children :: HashMap Event Leaf
  } deriving (Eq, Generic)

instance Show Leaf where
  show l = I.showLeaf
    -- use the generic show instance
    ((False,) . (:[]) . obs . body) (\l -> [V.singleton . count . body $ l] ) (HM.toList . children) "Parse" (obs . body $ l) l


-- instance Show Leaf where
--   show = go 1 " "
--     where
--       indent :: Int -> String
--       indent d = replicate (5 * d) ' '
--
--       showLeaf :: Int -> String -> LeafBody -> String
--       showLeaf d e b = "\n" ++ indent d ++ show e ++"->PLeaf{" ++ show b
--
--       go :: Int -> Event -> Leaf -> String
--       go d (T.unpack->e) (Leaf b cs)
--         | length cs == 0 = showLeaf d e b ++ ", no children}"
--         | otherwise = showLeaf d e b ++ "}\n"
--                       ++ indent (d + 1) ++ "children:"
--                       ++ printChilds d cs
--
--       printChilds :: Int -> HashMap Event Leaf -> String
--       printChilds d
--         = intercalate "\n"
--         . map (uncurry (go (d+1)))
--         . HM.toList

-- Lenses
bodyL :: Lens' Leaf LeafBody
bodyL = lens body $ \lf a -> lf { body = a }

childrenL :: Lens' Leaf (HashMap Event Leaf)
childrenL = lens children $ \lf a -> lf { children = a }

-- Smart Constructors
mkLeaf :: Integer -> Vector Event -> Leaf
mkLeaf c evts = Leaf (mkBody evts c mempty) mempty

newRoot :: Leaf
newRoot = mkLeaf 0 V.empty

newLeaf :: Vector Event -> Leaf
newLeaf = mkLeaf 1

-------------------------------------------------------------------------------

data LeafBody = LeafBody
  { obs       :: Vector Event
  , count     :: Integer
  , locations :: Locations
  } deriving (Eq, Generic)

mkBody :: Vector Event -> Integer -> Locations -> LeafBody
mkBody o c l = LeafBody o c l

obsL :: Lens' LeafBody (Vector Event)
obsL = lens obs $ \lf a -> lf { obs = a }

bodyObsL :: Lens' Leaf (Vector Event)
bodyObsL = bodyL . obsL

countL :: Lens' LeafBody Integer
countL = lens count $ \lf a -> lf { count = a }

bodyCountL :: Lens' Leaf Integer
bodyCountL = bodyL . countL

locationsL :: Lens' LeafBody (Locations)
locationsL = lens locations $ \lf a -> lf { locations = a }

bodyLocationsL :: Lens' Leaf (Locations)
bodyLocationsL = bodyL . locationsL

instance Show LeafBody where
  show (LeafBody o c _) =
    "obs: " ++ show o ++ ", count: " ++ show c ++ ", ls: " ++ "<>"

-------------------------------------------------------------------------------
-- Lenses for Parse Trees
-------------------------------------------------------------------------------
type instance Index Leaf = Vector Event
type instance IxValue Leaf = Leaf

-- |
--
-- Example:
--
-- >>> set (ix (V.fromList []) . bodyL . countL) 50000 newRoot
-- <BLANKLINE>
--      " "->PLeaf{obs: [], count: 50000, ls: <>, no children}
-- >>> set (ix (V.fromList ["9"]) . bodyL . countL) 50000 newRoot
-- <BLANKLINE>
--      " "->PLeaf{obs: [], count: 0, ls: <>, no children}
-- >>> set (ix (V.fromList ["9", "10"]) . bodyL . countL) 50000 newRoot
-- <BLANKLINE>
--      " "->PLeaf{obs: [], count: 0, ls: <>, no children}
instance Ixed Leaf where
  ix :: Vector Event -> Traversal' Leaf (IxValue Leaf)
  ix histories = go 0
    where
      go dpth f p@(Leaf bod childs)
        | V.length histories == dpth = f p
        | otherwise =
          case HM.lookup c childs of
            Nothing -> pure p
            Just child -> goAgain <$> go (dpth+1) f child
        where
          c :: Event
          c = histories ! dpth

          goAgain :: Leaf -> Leaf
          goAgain child' = Leaf bod (HM.insert c child' childs)

-- |
--
-- Example:
-- >>> navigate (Tree 1 newRoot) (V.fromList [])
-- Just
--      " "->PLeaf{obs: [], count: 0, ls: <>, no children}
-- >>> navigate (Tree 1 newRoot) (V.fromList ["test"])
-- Nothing
--
navigate :: Tree -> Vector Event -> Maybe Leaf
navigate tree history = view rootL tree ^? ix history
