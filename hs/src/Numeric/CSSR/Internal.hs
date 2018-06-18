module Numeric.CSSR.Internal where

import Protolude
import qualified Data.Tree.Conditional as Cond

type InferredDistribution = [(Cond.Leaf, Double)]

