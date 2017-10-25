{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
module Data.MTree.Parse where

import CSSR.Prelude.Mutable
import Control.Monad
import Data.Alphabet
import qualified Data.Tree.Parse as P

import qualified CSSR.Prelude.Vector   as V
import qualified Data.Text             as T
import qualified Data.HashSet          as HS (member, fromList, union)
import qualified Data.HashMap.Strict   as HM (insert, toList)
import qualified Data.HashTable.Class  as H (new, lookup, insert, toList)


data MLeaf s = MLeaf
  { obs      :: Vector Event
  , count    :: STRef s Integer
  , children :: HashTable s Event (MLeaf s)
  }

mkMLeaf :: Integer -> Vector Event -> ST s (MLeaf s)
mkMLeaf c evts = MLeaf
  <$> pure evts
  <*> newSTRef c
  <*> H.new

newRoot :: ST s (MLeaf s)
newRoot = mkMLeaf 0 V.empty

newLeaf :: Vector Event -> ST s (MLeaf s)
newLeaf = mkMLeaf 1

---------------------------------------------------------------------------------
-- |
-- Here's a given scenario:
--  * We encounter the history say "110"
--  * We go to the parse tree at the root
--  * We take the 1 child of the root
--  * We then take the 1 child of 0 (=10)
--  * We then take the 1 child of 10 (=110)
--
-- Examples
-- >>> :{
-- let node = runST $ do
--      r <- newRoot
--      addPath_ "110" r
--      freeze r
-- in
--   node
-- :}
-- <BLANKLINE>
--      " "->PLeaf{obs: [], count: 1, ls: <>}
--           children:
--           "1"->PLeaf{obs: ["1"], count: 1, ls: <>}
--                children:
--                "1"->PLeaf{obs: ["1","1"], count: 1, ls: <>}
--                     children:
--                     "0"->PLeaf{obs: ["1","1","0"], count: 1, ls: <>, no children}
--------------------------------------------------------------------------------
-- | helper function for addPath
addPath_ :: MLeaf s -> Text -> ST s ()
addPath_ l = addPath l . V.fromList . fmap T.singleton . T.unpack

addPath :: MLeaf s -> Vector Event -> ST s ()
addPath l events
  | V.null events = pure ()
  | otherwise     = walk (foldObs events) l
  where
    walk :: [Vector Event] -> MLeaf s -> ST s ()
    walk     [] l = modifySTRef (count l) (+1)
    walk (o:os) l = do
      modifySTRef (count l) (+1)
      H.lookup (children l) (nonEmptyHead o) >>= \case
        Just child -> walk os child
        Nothing    -> mkLf (o:os) l

    mkLf :: [Vector Event] -> MLeaf s -> ST s ()
    mkLf [] _ = pure ()
    mkLf (o:os) l = do
      lf <- newLeaf o
      H.insert (children l) (nonEmptyHead o) lf
      mkLf os lf

    nonEmptyHead :: Vector Event -> Event
    nonEmptyHead = fromMaybe (impossible msg) . V.head
      where
        msg :: String
        msg = "all vectors passed in are nonempty via `foldObs`"

foldObs :: Vector Event -> [Vector Event]
foldObs
  -- convert to a list for easier pattern matching
  = V.toList
  -- make sure you safely remove the empty list
  . fromMaybe mempty
  -- drop the empty list
  . V.tail
  -- continuously snoc on elements in a scan: [c, b, a] -> [[], [c], [b, c], [a, b, c]]
  . V.scanl' (\s x -> V.cons x s) mempty
  -- add each observation in from the least dependent to most dependent: [a, b, c] -> [c, b, a]
  . V.reverse


freeze :: forall s . MLeaf s -> ST s P.Leaf
freeze MLeaf{obs, count, children} = P.Leaf
  <$> mkBody
  <*> mkChildren
  where
    mkChildren :: ST s (HashMap Event P.Leaf)
    mkChildren = join $ fmap (foldrM step mempty) (H.toList children)

    step :: (Event, MLeaf s) -> HashMap Event P.Leaf -> ST s (HashMap Event P.Leaf)
    step (e, mlf) hm = HM.insert e <$> freeze mlf <*> pure hm

    mkBody :: ST s P.LeafBody
    mkBody = P.LeafBody obs <$> readSTRef count <*> pure mempty

buildMTree :: Int -> DataFileContents -> ST s (MLeaf s)
buildMTree n' (V.filter isValid -> cs) = do
  rt <- newRoot
  forM_ [0 .. V.length cs - n] (addPath rt . sliceEvents)
  return rt
  where
    n :: Int
    n = n' + 1

    sliceEvents :: Int -> Vector Event
    sliceEvents i
      | i + n <= length cs = V.slice i n cs               -- get all children of depth
      -- | i + n' <= length cs = trace "" $ V.slice i (length cs - i) cs   -- but also the depth
      | otherwise          = V.empty                      -- ignore all others

isValid :: Event -> Bool
isValid e = not . HS.member e . HS.fromList $ ["\r", "\n"]

-------------------------------------------------------------------------------
-- Build a Parse Tree and get Alphabet
-------------------------------------------------------------------------------

buildTree :: Int -> DataFileContents -> P.Tree
buildTree n df = P.Tree n (runST $ buildMTree n df >>= freeze)

getAlphabet :: P.Tree -> Alphabet
getAlphabet (P.Tree _ rt) = mkAlphabet $ go mempty [rt]
 where
  go :: HashSet Event -> [P.Leaf] -> HashSet Event
  go es [] = es
  go es (topairs->ps) = go (HS.union es (HS.fromList . map fst $ ps)) (map snd ps)

  topairs :: [P.Leaf] -> [(Event, P.Leaf)]
  topairs = concatMap (HM.toList . view P.childrenL)


