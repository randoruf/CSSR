{-# LANGUAGE ScopedTypeVariables #-}
module CSSR.Statistics.Complexity where

import CSSR.Prelude
import Data.Vector as V


-- I think this is the Grassberger-Crutchfield-Young "statistical complexity." I'm taking a shot
-- in the dark at what the name might be and the authors. This statistical complexity can be seen
-- as the Shannon entropy of the distribution over causal states.
cMu :: forall f . (Num f, Floating f) => Vector f -> f
cMu dist = (-1) * V.foldl' go 0 dist
  where
    go :: f -> f -> f
    go cMu p = cMu + (p * log2 p)


complexity :: (Num f, Floating f) => Vector f -> f
complexity dist = (-1) * V.sum (V.map (\p -> p * log2 p) dist)

