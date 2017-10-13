module Data.Tree.InternalSpec where

import CSSR.Prelude.Test
import Data.Tree.Internal

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "getAncestors on 0-100, 0 being oldest" $ do
    let ancestors = getAncestors mkParent 100
    it "should not include itself" $
      ancestors `shouldNotContain` [100]
    it "should be in order of oldest last" $
      take 10 ancestors `shouldBe` [0..9]
    it "should return the empty list when no parent is found" $
      getAncestors (const Nothing) (undefined::Int) `shouldBe` []
  where
    mkParent :: Int -> Maybe Int
    mkParent i = if i <= 0 then Nothing else Just (i-1)
