{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
module Data.Tree.Parse where

import Data.List
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import Lens.Micro.Internal

import CSSR.Prelude

-------------------------------------------------------------------------------
-- Parse Tree ADTs
-------------------------------------------------------------------------------
data Tree = Tree
  { depth :: Int
  , root :: Leaf
  } deriving (Eq)

rootL :: Lens' Tree Leaf
rootL = lens root $ \lf a -> lf { root = a }

depthL :: Lens' Tree Int
depthL = lens depth $ \lf a -> lf { depth = a }

instance Show Tree where
  show (Tree d r) = "ParseTree{depth: " ++ show d ++ "}\n  root:" ++ show r

-------------------------------------------------------------------------------

data Leaf = Leaf
  { body :: LeafBody
  , children :: HashMap Event Leaf
  } deriving (Eq)

bodyL :: Lens' Leaf LeafBody
bodyL = lens body $ \lf a -> lf { body = a }

childrenL :: Lens' Leaf (HashMap Event Leaf)
childrenL = lens children $ \lf a -> lf { children = a }


instance Show Leaf where
  show = go 1 " "
    where
      indent :: Int -> String
      indent d = replicate (5 * d) ' '

      showLeaf :: Int -> Event -> LeafBody -> String
      showLeaf d e b = "\n" ++ indent d ++ show e ++"->PLeaf{" ++ show b

      go :: Int -> Event -> Leaf -> String
      go d e (Leaf b cs)
        | length cs == 0 = showLeaf d e b ++ ", no children}"
        | otherwise = showLeaf d e b ++ "}\n"
                      ++ indent (d + 1) ++ "children:"
                      ++ printChilds d cs

      printChilds :: Int -> HashMap Event Leaf -> String
      printChilds d = intercalate "\n" . map (uncurry (go (d+1))) . HM.toList

-------------------------------------------------------------------------------

data LeafBody = LeafBody
  { obs       :: Vector Event
  , count     :: Integer
  , locations :: Locations
  } deriving (Eq)

mkBody :: Monad m => Vector Event -> m Integer -> Locations -> m LeafBody
mkBody o mc l = (LeafBody o) <$> mc <*> pure l

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

-- Example: set (ix (V.fromList "9") . body . count)  50000 mkRoot
--
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

navigate :: Tree -> Vector Event -> Maybe Leaf
navigate tree history = view rootL tree ^? ix history
