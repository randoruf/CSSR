-------------------------------------------------------------------------------
-- |
-- Module    :  CSSR.Results.GraphViz
-- Copyright :  (c) Sam Stites 2017
-- License   :  BSD3
-- Maintainer:  sam@stites.io
-- Stability :  experimental
-- Portability: non-portable
--
-- Report found state machines in graphviz.
--
-- FIXME: use https://hackage.haskell.org/package/graphviz
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CSSR.Results.GraphViz where

import Protolude
import Data.Text (Text)
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Char as C

import Data.CSSR.State
import qualified Data.CSSR.State as CSSR

-- | Global style of graphviz
dotMeta :: Text
dotMeta = T.unlines
  [ "size = \"6,8.5\";"
  , "ratio = \"fill\";"
  , "node [shape = circle];"
  , "node [fontsize = 24];"
  , "edge [fontsize = 24];"
  ]

-- | render graphviz
render
  :: Text  -- ^ file path (possibly "name of task")
  -> [CSSR.State]
  -> Text  -- ^ file as Text
render fp ss = T.unlines
  [ "digraph \"" <> fp <> "\" {"
  , dotMeta
  , dotInfo ss
  ]

dotInfo :: [CSSR.State] -> Text
dotInfo ss
  = T.unlines
  $ map (uncurry renderState)
  $ zip [0..] ss
 where
  renderState :: StateIx -> CSSR.State -> Text
  renderState ix s = undefined
      -- case (state, i) =>
      --   val sTransitions:Map[Event, TransitionState] = allStates.transitionMap(state)
      --   state.distribution
      --     .toArray
      --     .view.zipWithIndex
      --     .foldLeft[String]("") {
      --     case (memo, (prob, k)) if prob <= 0 => memo
      --     case (memo, (prob, k)) =>
      --       val symbol:Event = alphabet.raw(k)
      --       sTransitions(symbol) match {
      --         case None => memo
      --         case Some(transition) =>
      --           memo + s"""${idxAsStr(i)} -> ${idxAsStr(allStates.stateMap(transition))} [label = "$symbol: ${"%.7f".format(prob)}"];\n"""
      --       }
      --   }

-- | Word representation of a state's index
newtype StateIx = StateIx { unStateIx :: Word }
  deriving (Num, Real, Ord, Integral, Show, Bounded, Enum, Eq)

-- | Which style of label we would like to use
data StateLabels
  = Ascii
  | Numeric

-- | How to label a state
labelState :: StateIx -> StateLabels -> Text
labelState ix = \case
  Ascii   -> intToAscii ix
  Numeric -> T.pack . show $ ix
 where
  intToAscii :: StateIx -> Text
  intToAscii = T.singleton . C.intToDigit . fromIntegral . (+ 17)


