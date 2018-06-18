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
{-# LANGUAGE TupleSections #-}
module CSSR.Results.GraphViz where

import Protolude hiding (State, Symbol)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.HashSet (HashSet)
import Data.Vector (Vector)
import Data.HashMap.Strict (HashMap)
import Numeric (showFFloat)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Char as C
import qualified Data.HashMap.Strict as HM

import Data.CSSR.Alphabet
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
  -> Alphabet
  -> StateLabels
  -> Vector State
  -> Text  -- ^ file as Text
render fp a sl ss = T.unlines
  [ "digraph \"" <> fp <> "\" {"
  , dotMeta
  , dotInfo a sl ss
  ]

dotInfo :: Alphabet -> StateLabels -> Vector State -> Text
dotInfo a sl ss
  = T.unlines
  $ zipWith renderState [0..] (toList ss)
 where
  vs :: Vector State
  vs = V.fromList $ toList ss

  renderState :: StateIx -> CSSR.State -> Text
  renderState ix s = T.unlines $ renderDist sDist
    where
      renderDist :: HashMap Event (Double, StateIx) -> [Text]
      renderDist d = go <$> HM.toList d
        where
          go :: (Event, (Double, StateIx)) -> Text
          go (evt, (p, s)) = T.concat
            [ labelState ix sl, " -> ", labelState s sl
            , "[label = ", evt, ": ", T.pack $ showFFloat (Just 4) p "", "];"
            ]

      sDist :: HashMap Event (Double, StateIx)
      sDist = HM.mapMaybeWithKey getTransition (distributionsLookup a ss s)

      getTransition :: Event -> Double -> Maybe (Double, StateIx)
      getTransition e p
        | p <= 0    = Nothing
        | otherwise = (p,) <$> (join (HM.lookup e sTransitions) >>= getIx vs)

      sTransitions :: HashMap Event (Maybe CSSR.State)
      sTransitions = allTransitionsLookup a ss s


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

getIx :: V.Vector State -> State -> Maybe StateIx
getIx ss s = fromIntegral <$> V.findIndex (== s) ss

