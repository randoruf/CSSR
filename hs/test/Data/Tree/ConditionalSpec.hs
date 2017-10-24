module Data.Tree.ConditionalSpec where

import qualified Data.Vector as V
import qualified Data.Text as T

import CSSR.Prelude.Test
import Data.Tree.Conditional
import qualified Data.MTree.Parse as MParse (getAlphabet, buildTree)
import qualified Data.Tree.Parse as Parse
import qualified Data.Tree.ParseSpec as Parse (tree) -- use the tree from the parse spec for testing

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "converting a parse tree to a conditional tree" $ do
    it "removes the last children from a Parse Tree" $
      all (isNothing . navigate tree) $ fmap txt2evts ["abc", "bcc"]

    it "keeps the children of last depth" $
      all (isJust . navigate tree) $ fmap txt2evts ["ab", "bc"]

  where
    txt2evts :: Text -> Vector Event
    txt2evts = V.fromList . fmap T.singleton . T.unpack

    tree :: Tree
    tree = convert Parse.tree (MParse.getAlphabet Parse.tree)


