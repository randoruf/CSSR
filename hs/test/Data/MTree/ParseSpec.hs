{-# LANGUAGE ScopedTypeVariables #-}
module Data.MTree.ParseSpec where

import Data.MTree.Parse

import Data.Alphabet

import qualified Data.Tree.Parse  as P
import qualified Data.HashSet     as HS
import qualified Data.Vector      as V
import qualified Data.Text        as T

import CSSR.Prelude.Mutable
import CSSR.Prelude.Test
import Test.QuickCheck (property)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "addPath"     addPathSpec
  describe "addPath_"    addPath_Spec
  describe "buildTree"   buildTreeSpec
  describe "getAlphabet" alphabetSpec

-------------------------------------------------------------------------------

addPathSpec :: Spec
addPathSpec =
  describe "when we encounter the history 110" $ do
    let rt = runST $ mkLeaf (addPath (V.fromList $ T.singleton <$> "110"))
    it "keeps the root node" $ view (P.bodyL . P.obsL) rt == V.empty
    it "bumps the root node count" $ view (P.bodyL . P.countL) rt == 1

    childChecks "root" rt "0" (V.fromList $ T.singleton <$> "0") 1

    let _0 = findLeaf rt "0"
    childChecks (show "0") _0 "1" (V.fromList $ T.singleton <$> "10") 1

    let _10 = findLeaf _0 "1"
    childChecks (show "10") _10 "1" (V.fromList $ T.singleton <$> "110") 1


addPath_Spec :: SpecWith (Arg Bool)
addPath_Spec =
  it "is identical to addPath of a String" $ property $ \str ->
    let
      lp  = runST $ mkLeaf (addPath (V.fromList $ fmap T.singleton str))
      lp' = runST $ mkLeaf (addPath_ (T.pack str))
    in
      lp == lp'


mkLeaf :: (MLeaf s -> ST s ()) -> ST s P.Leaf
mkLeaf mutate = freeze =<< do
  r <- newRoot
  mutate r
  pure r


-------------------------------------------------------------------------------

buildTreeSpec :: Spec
buildTreeSpec = do
  describe "when we build the tree \"abcc\" with depth 2" $ do
    let tree = buildTree 2 (V.fromList $ T.singleton <$> "abcc")
    it "the tree has depth 2" $
      view P.depthL tree == 2

    let rt = view P.rootL tree
    it "the root node sees 3 traversals" $ view (P.bodyL . P.countL) rt == 3

    childChecks "root" rt "c" (V.fromList ["c"]) 3
    noChildTest "root" rt "a"
    noChildTest "root" rt "b"

    let _c = findLeaf rt "c"
    childChecks (show "c") _c "b" (V.fromList $ T.singleton <$> "bc") 1
    childChecks (show "c") _c "c" (V.fromList $ T.singleton <$> "cc") 1

    let _cc = findLeaf _c "c"
    childChecks (show "cc") _cc "b" (V.fromList $ T.singleton <$> "bcc") 1

    noChildrenTest (show "bcc") $ findLeaf _cc "b"

    let _bc = findLeaf _c "b"
    childChecks (show "bc") _bc "a" (V.fromList $ T.singleton <$> "abc") 1
    noChildrenTest (show "abc") $ findLeaf _bc "a"

alphabetSpec :: Spec
alphabetSpec =
  it "building a tree from \"abcc\" finds the correct alphabet" $ do
    let alpha = getAlphabet $ buildTree 2 (V.fromList $ T.singleton <$> "abcc")
    V.all (\s -> HS.member s $ HS.fromList  $ T.singleton <$> "abc") (idxToSym alpha)

-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------


-- | General Tests for a child node
childChecks :: String -> P.Leaf -> Event -> Vector Event -> Integer -> Spec
childChecks name lf nxt full c =
  describe ("the "++ name ++ " node") $ do
    let child = view P.childrenL lf ^? ix nxt
    let full' = V.toList full
    it ("has a(n) "++ show nxt ++" child") $ isJust child
    it ("has a(n) "++ show nxt ++" child with a count of " ++ show c) $
      maybe False ((== c) . view (P.bodyL . P.countL)) child
    it (show nxt ++" child has observation "++ show full') $
      maybe False ((== full) . view (P.bodyL . P.obsL)) child


-- | Inverse of @childChecks@
noChildTest :: String -> P.Leaf -> Event -> Spec
noChildTest name lf nxt =
  it ("the "++ name ++ " node has no "++ show nxt ++" child") $
    isNothing (view P.childrenL lf ^? ix nxt)

noChildrenTest :: String -> P.Leaf -> Spec
noChildrenTest name lf =
  it ("the "++ name ++ " node has no children") $
    length (view P.childrenL lf) == 0



-- | Find the leaf, or an invalid node which should not pass any property-checks
findLeaf :: P.Leaf -> Event -> P.Leaf
findLeaf lf o = fromMaybe emptyLeaf (view P.childrenL lf ^? ix o)
  where
    emptyLeaf :: P.Leaf
    emptyLeaf = P.Leaf (P.LeafBody mempty 0 mempty) mempty


