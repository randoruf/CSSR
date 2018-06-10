{-# LANGUAGE NamedFieldPuns #-}
module CSSR.AllStates where

import CSSR.Prelude
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Tree.Looping as L
import Data.Map (Map)

import Data.Alphabet

data State = State
  { transitions :: HashMap Event State
  , terminal :: L.Leaf
  } deriving (Show, Eq)

transitionTo :: State -> Event -> Maybe State
transitionTo = flip HM.lookup . transitions

notNull :: State -> Bool
notNull = not . null . L.path . terminal

instance Hashable State where
  hashWithSalt s st =
    hashWithSalt s (termpath st, HM.map termpath (transitions st))
   where
    termpath :: State -> Vector Event
    termpath = L.path . terminal

type AllStates = [State]

frequency :: Alphabet -> AllStates -> HashMap State (HashMap Symbol Integer)
frequency alpha allstates = HM.fromList (ssize <$> allstates)
 where
  ssize :: State -> (State, HashMap Symbol Integer)
  ssize s = (s, HM.map (\i -> freqs ! i) (symToIdx alpha))
   where
    freqs :: Vector Integer
    freqs = tofreqs . L.body . terminal $ s

    tofreqs :: Either L.Leaf L.LeafBody -> Vector Integer
    tofreqs = either (impossible "states do not loop") L.frequency


distributions :: Alphabet -> AllStates -> HashMap State (HashMap Symbol Double)
distributions alpha allstates = HM.fromList $ map toprobs freq
  where
    freq :: [(State, HashMap Symbol Integer)]
    freq = HM.toList (frequency alpha allstates)

    toprobs :: (State, HashMap Symbol Integer) -> (State, HashMap Symbol Double)
    toprobs (s, hms) = (s, HM.map ((/ (fromIntegral . sum . HM.elems $ hms)) . fromIntegral) hms)

distributionsLookup :: Alphabet -> AllStates -> State -> HashMap Event Double
distributionsLookup alpha allstates s =
  HM.lookupDefault (impossible "all states should be accounted for") s $
    distributions alpha allstates

distributionLookup :: Alphabet -> AllStates -> State -> Double
distributionLookup alpha allstates s =
  HM.lookupDefault (impossible "all symbols should be accounted for") s $
    distribution alpha allstates

distribution :: Alphabet -> AllStates -> HashMap State Double
distribution alpha allstates = HM.map ((/total) . fromIntegral) freq
 where
  freq :: HashMap State Integer
  freq = HM.map (sum . HM.elems) (frequency alpha allstates)

  total :: Double
  total = fromIntegral $ sum (HM.elems freq)

--   val states      = eqClasses.toArray
--   val transitions:Array[HashMap[Event, TransitionState]] = states.map{ state => transitionHashMap(state) }

--   val stateIndexes:Array[Set[Int]]        = states.map{_.histories.flatHashMap{_.locations.keySet}.toSet}
--   val stateHashMap:HashMap[State, Int] = states.zipWithIndex.toHashMap

--   val frequency:DenseVector[Double]    = new DenseVector[Double](stateIndexes.map{_.size.toDouble})
--   val distribution:DenseVector[Double] = frequency :/ sum(frequency)
-- }

