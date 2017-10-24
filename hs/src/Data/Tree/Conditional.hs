{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.Tree.Conditional where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV

import CSSR.Prelude
import CSSR.Probabilistic (Probabilistic)
import Data.Alphabet
import qualified CSSR.Probabilistic as Prob
import qualified Data.Tree.Internal as I
import qualified Data.Tree.Parse as P
import qualified Data.Tree.Parse as Parse


-------------------------------------------------------------------------------
-- Conditional Tree ADTs
-------------------------------------------------------------------------------
data Tree = Tree
  { depth :: Int
  , alphabet :: Alphabet
  , root :: Leaf
  } deriving (Eq, Generic, NFData)

data Leaf = Leaf
  { body :: LeafBody
  , children :: HashMap Event Leaf
  } deriving (Eq, Generic, NFData)

data LeafBody = LeafBody
  { obs       :: Vector Event
  , frequency :: Vector Integer
  } deriving (Eq, Generic, NFData)

instance Show Tree where
  show (Tree d a r) = "ConditionalTree {depth " ++ show d ++ ", "++ show a ++"}\n  root:" ++ show r

instance Show Leaf where
  show l = I.showLeaf
    -- use the generic show instance
    ((False,) . (:[]) . view lobsL) ((:[]) . view lfrequencyL) (HM.toList . view childrenL) "Cond" (view lobsL l) l

instance Show LeafBody where
  show (LeafBody o f) = "obs: " ++ show o ++ ", freq: " ++ show f ++ ", dist:" ++ show (Prob.freqToDist f)

instance Hashable LeafBody
instance Hashable Leaf
instance Hashable Tree

depthL :: Lens' Tree Int
depthL = lens depth $ \leaf a -> leaf { depth = a }

alphabetL :: Lens' Tree Alphabet
alphabetL = lens alphabet $ \leaf a -> leaf { alphabet = a }

rootL :: Lens' Tree Leaf
rootL = lens root $ \leaf a -> leaf { root = a }

bodyL :: Lens' Leaf LeafBody
bodyL = lens body $ \leaf a -> leaf { body = a }

childrenL :: Lens' Leaf (HashMap Event Leaf)
childrenL = lens children $ \leaf a -> leaf { children = a }

obsL :: Lens' LeafBody (Vector Event)
obsL = lens obs $ \bod a -> bod { obs = a }

frequencyL :: Lens' LeafBody (Vector Integer)
frequencyL = lens frequency $ \a b -> a { frequency = b }

lobsL :: Lens' Leaf (Vector Event)
lobsL = bodyL . obsL

lfrequencyL :: Lens' Leaf (Vector Integer)
lfrequencyL = bodyL . frequencyL


instance Probabilistic Leaf where
  frequency = GV.convert . view (bodyL . frequencyL)


-------------------------------------------------------------------------------
-- Convert ParseTree to Tree
-------------------------------------------------------------------------------

convert :: P.Tree -> Alphabet -> Tree
convert (P.Tree d rt) alpha = trace (show rt) $ Tree d alpha (go d rt)
  where
    go :: Int -> P.Leaf -> Leaf
    go 0 lf = mkLeaf lf mempty
    go d lf = mkLeaf lf $ HM.map (go (d-1)) (view P.childrenL lf)

    mkLeaf :: P.Leaf -> HashMap Event Leaf -> Leaf
    mkLeaf (P.Leaf (P.LeafBody o _ _) cs) = Leaf (mkBody o cs)

    mkBody :: Vector Event -> HashMap Event P.Leaf -> LeafBody
    mkBody o cs = LeafBody o (mkFrequency cs alpha)

    mkFrequency :: HashMap Event P.Leaf -> Alphabet -> Vector Integer
    mkFrequency cs (Alphabet vec _) =
      V.map (\s -> (maybe 0 getCounts . HM.lookup s) cs) vec

    getCounts :: P.Leaf -> Integer
    getCounts = view (P.bodyL . P.countL)


-------------------------------------------------------------------------------
-- CSSR Properties
-------------------------------------------------------------------------------
--
-- Excisability:
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

-------------------------------------------------------------------------------
-- Lenses for Hist Trees
-------------------------------------------------------------------------------
type instance Index Leaf = Vector Event
type instance IxValue Leaf = Leaf

-- Example: set (ix (V.fromList "9") . body . count)  50000 mkRoot
--
instance Ixed Leaf where
  ix :: Vector Event -> Traversal' Leaf (IxValue Leaf)
  ix histories = go (V.length histories - 1)
    where
      go 0 f p = f p
      go d f p@(Leaf bod childs)
        | d <= 0 = f p
        | otherwise =
          case HM.lookup c childs of
            Nothing -> pure p
            Just child -> goAgain <$> go (d-1) f child
        where
          c :: Event
          c = histories V.! d

          goAgain :: Leaf -> Leaf
          goAgain child' = Leaf bod (HM.insert c child' childs)

navigate :: Tree -> Vector Event -> Maybe Leaf
navigate (view rootL -> rt) history = I.navigate (\ls e -> HM.lookup e (view childrenL ls)) rt history

-------------------------------------------------------------------------------
-- Show helper functions
-------------------------------------------------------------------------------

showAllObs :: [Leaf] -> String
showAllObs = T.unpack . T.intercalate "," . fmap (T.concat . V.toList . obs . body)

showAllDists :: [Leaf] -> String
showAllDists = show . fmap (intercalate "," . V.toList . fmap f'4 . Prob.freqToDist . frequency . body)

