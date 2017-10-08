{-# LANGUAGE RankNTypes #-}
module Data.Tree.Internal where

import CSSR.Prelude
import CSSR.Probabilistic (Probabilistic)
import qualified CSSR.Probabilistic as Prob

-- class Probabilistic l => CSSRLeaf l where

-- | === Homogeneity
-- Psuedocode from paper:
--   INPUTS: looping node, parse tree
--   COLLECT all next-step histories from looping node in parse tree
--   FOR each history in next-step histories
--     FOR each child in history's children
--       IF child's distribution ~/=  node's distribution
--       THEN RETURN false
--       ENDIF
--     ENDFOR
--   ENDFOR
--   RETURN TRUE
--
isHomogeneous
  :: (Probabilistic parent, Foldable hists, Probabilistic child)
  => Double
  -> Lens' parent (hists child)
  -> (child -> Vector Integer)
  -> parent
  -> Bool
isHomogeneous sig childrenL child2dist parent =
  all (cMatchesP . child2dist)
  . toList
  . view childrenL
  $ parent

 where
  cMatchesP :: Vector Integer -> Bool
  cMatchesP cdist = Prob.matchesDists_ (Prob.frequency parent) cdist sig




