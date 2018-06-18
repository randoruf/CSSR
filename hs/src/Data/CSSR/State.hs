-------------------------------------------------------------------------------
-- |
-- Module    :  Data.CSSR.State
-- Copyright :  (c) Sam Stites 2017
-- License   :  BSD3
-- Maintainer:  sam@stites.io
-- Stability :  experimental
-- Portability: non-portable
--
-- Representations of CSSR State types
-------------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}
module Data.CSSR.State where

import Protolude hiding (State, Symbol)

import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Vector (Vector, (!))

import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import qualified Data.Tree.Looping as L
import Data.CSSR.Alphabet

-- | type alias of how CSSR prefers to handle the aggregation of states
type AllStates = HashSet State


-- | A 'State' in CSSR is a group of terminal leaves, which form an equivalence
-- class, and all valid transitions out of the state into another state.
data State = State
  { transitions :: HashMap Event State
  , terminal :: L.Leaf -- ^ FIXME: this should be @HashSet L.Leaf@ which form an eq-class
  } deriving (Show, Eq)

instance Hashable State where
  hashWithSalt s st =
    hashWithSalt s (termpath st, HM.map termpath (transitions st))
   where
    termpath :: State -> Vector Event
    termpath = L.path . terminal

-- | given a state and a valid event, attempt to transition to a new state
transitionTo :: State -> Event -> Maybe State
transitionTo = flip HM.lookup . transitions

-- | check to see if a state's terminal node represents the root node.
isRoot :: State -> Bool
isRoot = V.null . L.path . terminal

-- * Distribution-based properties of all states

-- | Given the alphabet space and all states, return the global frequencies
-- of each state
frequency :: Foldable f => Alphabet -> f State -> HashMap State (HashMap Symbol Integer)
frequency alpha = HM.fromList . fmap ssize . toList
 where
  ssize :: State -> (State, HashMap Symbol Integer)
  ssize s = (s, HM.map (\i -> freqs ! i) (symToIdx alpha))
   where
    freqs :: Vector Integer
    freqs = tofreqs . L.body . terminal $ s

    tofreqs :: Either L.Leaf L.LeafBody -> Vector Integer
    tofreqs = either (panic "states do not loop") L.frequency

frequencyLookup :: Foldable f => Alphabet -> f State -> State -> HashMap Event Integer
frequencyLookup alpha allstates s =
  HM.lookupDefault (panic "all states should be accounted for") s $
    frequency alpha allstates

distributions :: Foldable f => Alphabet -> f State -> HashMap State (HashMap Symbol Double)
distributions alpha allstates = HM.fromList $ map toprobs freq
  where
    freq :: [(State, HashMap Symbol Integer)]
    freq = HM.toList (frequency alpha allstates)

    toprobs :: (State, HashMap Symbol Integer) -> (State, HashMap Symbol Double)
    toprobs (s, hms) = (s, HM.map ((/ (fromIntegral . sum . HM.elems $ hms)) . fromIntegral) hms)

distributionsLookup :: Foldable f => Alphabet -> f State -> State -> HashMap Event Double
distributionsLookup alpha allstates s =
  HM.lookupDefault (panic "all states should be accounted for") s $
    distributions alpha allstates

distribution :: Foldable f => Alphabet -> f State -> HashMap State Double
distribution alpha allstates = HM.map ((/total) . fromIntegral) freq
 where
  freq :: HashMap State Integer
  freq = HM.map (sum . HM.elems) (frequency alpha allstates)

  total :: Double
  total = fromIntegral $ sum (HM.elems freq)

distributionLookup :: Foldable f => Alphabet -> f State -> State -> Double
distributionLookup alpha allstates s =
  HM.lookupDefault (panic "all symbols should be accounted for") s $
    distribution alpha allstates

allTransitions :: Foldable f => Alphabet -> f State -> HashMap State (HashMap Event (Maybe State))
allTransitions alpha as =
  HM.fromList (fulltransitions <$> toList as)
 where
   symMap = symToIdx alpha
   lookupT s k = HM.lookup k (transitions s)

   fulltransitions s = (s, HM.mapWithKey (const . lookupT s) symMap)

allTransitionsLookup :: Foldable f => Alphabet -> f State -> State -> HashMap Event (Maybe State)
allTransitionsLookup alpha as s =
  HM.lookupDefault (panic "all states should be accounted for") s $
    allTransitions alpha as

--   val states      = eqClasses.toArray
--   val transitions:Array[HashMap[Event, TransitionState]] = states.map{ state => transitionHashMap(state) }

--   val stateIndexes:Array[Set[Int]]        = states.map{_.histories.flatHashMap{_.locations.keySet}.toSet}
--   val stateHashMap:HashMap[State, Int] = states.zipWithIndex.toHashMap
-- }


