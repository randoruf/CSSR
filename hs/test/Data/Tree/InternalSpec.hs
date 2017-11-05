module Data.Tree.InternalSpec where

import CSSR.Prelude.Test
import Data.Tree.Internal

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "getAncestors" $ do
    it "should return the empty list when no parent is found" $
      getAncestors (const Nothing) (undefined::Int) `shouldBe` []

    describe "0-100, 0 being oldest" $ do
      let
        mkParent :: Int -> Maybe Int
        mkParent i = if i <= 0 then Nothing else Just (i-1)

        ancestors :: [Int]
        ancestors = getAncestors mkParent 100

      it "should not include itself" $
        ancestors `shouldNotContain` [100]
      it "should be in order of oldest last" $
        take 10 ancestors `shouldBe` [0..9]

    describe "on [[],[1],[1,2],[1,2,3]..[1..10]], [] being oldest" $ do
      let
        mkParent :: [Int] -> Maybe [Int]
        mkParent   [] = Nothing
        mkParent ints = Just $ take (length ints - 1) ints

        youngest :: [Int]
        youngest = [1..10]

        ancestors :: [[Int]]
        ancestors = getAncestors mkParent youngest

      it "should not include itself" $
        ancestors `shouldNotContain` [youngest]

      it "should be in order of oldest last" $
        take 3 ancestors `shouldBe` [[], [1], [1,2]]

