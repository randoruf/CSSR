module CSSR.Algorithm.Phase2Spec where

import CSSR.Prelude.Test
import CSSR.Prelude.Mutable (runST)

import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

import CSSR.Fixtures (short_ep)
import CSSR.Algorithm.Phase1 (initialization)
import CSSR.Algorithm.Phase2 (grow)
import Data.Alphabet
import Data.Tree.Hist

import qualified Data.MTree.Looping as ML


main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "a short even process" $ do
    let ltree = runST $ grow 0.01 (initialization 2 short_ep) >>= ML.freezeTree
    it "should be tested" $
      pending

  where
    getChildren :: [Leaf] -> [Leaf]
    getChildren = foldr ((<>) . HM.elems . children) []

-- Leaf{fromList [
--      " "->Leaf{obs: [], freq: [28,42]}
--           children:
--           "0"->Leaf{obs: ["0"], freq: [1,1], no children}
-- <BLANKLINE>
--           "1"->Leaf{obs: ["1"], freq: [1,1], no children}], [0,0], fromList []}
--

-- |
-- Example:
--
-- >>> import CSSR.Algorithm.Phase1
-- >>> let short_ep = "00011110001100011110000111101101111111111000110001101101100111100111100"
-- >>> let htree = initialization 1 short_ep
-- >>> runST $ grow 0.01 htree >>= ML.freeze
-- Leaf{fromList [
--      " "->Leaf{obs: [], freq: [28,42]}
--           children:
--           "0"->Leaf{obs: ["0"], freq: [1,1], no children}
-- <BLANKLINE>
--           "1"->Leaf{obs: ["1"], freq: [1,1], no children}], [0,0], fromList []}



