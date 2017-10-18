{-# LANGUAGE ScopedTypeVariables #-}
module CSSR.Statistics.CMu where

import CSSR.Prelude
import Data.Vector as V

-- I think this is the Grassberger-Crutchfield-Young "statistical complexity."
-- I'm taking a shot in the dark at what the name might be.
cMu :: forall f . (Num f, Floating f) => Vector f -> f
cMu dist = (-1) * V.foldl' go 0 dist
  where
    go :: f -> f -> f
    go cMu p = cMu + (p * log2 p)
