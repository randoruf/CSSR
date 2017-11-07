module Data.Tree.ParseSpec where

import qualified Data.Vector as V
import qualified Data.Text as T

import CSSR.Prelude.Test
import Data.Tree.Parse
import qualified Data.MTree.Parse as MParse (buildTree)

main :: IO ()
main = hspec spec

tree :: Tree
tree = MParse.buildTree 2 (V.fromList $ T.singleton <$> "abcc")

spec :: Spec
spec =
  describe "buildTree and navigation" $ do
    describe "depth 0" $
      tree `hasPath` []

    describe "depth 1" $ do
      tree `hasPath` ["c"]
      tree `doesn'tHave` ["a"]
      tree `doesn'tHave` ["b"]

    describe "depth 2" $ do
      tree `hasPath` ["b","c"]
      tree `hasPath` ["c","c"]
      tree `doesn'tHave` ["a","b"]

    describe "depth 3" $ do
      tree `hasPath` ["a","b","c"]
      tree `hasPath` ["b","c","c"]

  where
    hasPath :: Tree -> [Event] -> Spec
    hasPath tree path = do
      let node = findNode tree path
      it ("finds node " ++ show path) $ isJust node
      it ("node " ++ show path ++ "'s path matches") $
        maybe False ((V.fromList path ==) . view (bodyL . obsL)) node

    doesn'tHave :: Tree -> [Event] -> Spec
    doesn'tHave tree path =
      it ("fails to find node " ++ show path) $
        isNothing . navigate tree . V.fromList $ path

    findNode :: Tree -> [Event] -> Maybe Leaf
    findNode tree path = navigate tree . V.fromList $ path

