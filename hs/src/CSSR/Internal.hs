module CSSR.Internal
  ( statesAndTransitions
  , pathToState
  ) where

import CSSR.Prelude

import Data.Alphabet (Alphabet(..))

import Data.Tree.Looping (Tree, Terminal, Leaf)
import qualified Data.Tree.Looping as L

import qualified Data.Vector as V
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM

statesAndTransitions
  :: Alphabet
  -> Tree
  -> (HashSet Terminal, HashMap Terminal (HashMap Symbol (Maybe Terminal)))
statesAndTransitions alpha looping = (terms, mapTransitions terms)
 where
  terms :: HashSet L.Terminal
  terms = L.terminals looping

  mapTransitions :: HashSet L.Terminal -> HashMap Terminal (HashMap Symbol (Maybe Terminal))
  mapTransitions terms = mapHashKeys transitions (HS.toMap terms)

  transitions :: Leaf -> HashMap Symbol (Maybe Terminal)
  transitions t = HM.mapWithKey (curry $ pathToState looping t) (symToIdx alpha)


pathToState :: Tree -> Terminal -> (Event, Int) -> Maybe Terminal
pathToState tree t0 (evt, idx) = do
  let term = fromMaybe dont (preview (L.bodyL . _Right) t0)
  guard (L.frequency term V.! idx /= 0)
  l <- L.navigate' (L.root tree) wa
  guard (l `HS.member` L.terminals tree)
  pure l
 where
  wa :: Vector Event
  wa = L.path t0 `V.snoc` evt

  dont = impossible "terminals don't loop!"
