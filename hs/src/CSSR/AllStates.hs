module CSSR.AllStates where

import CSSR.Prelude
import Data.Alphabet (Alphabet)
import qualified Data.HashMap.Strict as HM

type State = ()
type Symbol = Text

type AllStates = HashMap State (HashMap Symbol (State))

frequency :: AllStates -> HashMap State (HashMap Symbol Integer)
frequency allstates = HM.mapWithKey ssize allstates
  where
    ssize :: State -> HashMap Symbol State -> HashMap Symbol Integer
    ssize s _ = undefined

distribution :: AllStates -> HashMap State (HashMap Symbol Double)
distribution allstates = HM.fromList $ map toprobs freq
  where
    freq :: [(State, HashMap Symbol Integer)]
    freq = HM.toList (frequency allstates)

    fsum :: [(State, Double)]
    fsum = map (identity *** fromIntegral . sum . HM.elems) freq

    toprobs :: (State, HashMap Symbol Integer) -> (State, HashMap Symbol Double)
    toprobs (s, hms) = (s, HM.map ((/ (fromIntegral . sum . HM.elems $ hms)) . fromIntegral) hms)


--   val states      = eqClasses.toArray
--   val transitions:Array[Map[Event, TransitionState]] = states.map{ state => transitionMap(state) }

--   val stateIndexes:Array[Set[Int]]        = states.map{_.histories.flatMap{_.locations.keySet}.toSet}
--   val stateMap:Map[State, Int] = states.zipWithIndex.toMap

--   val frequency:DenseVector[Double]    = new DenseVector[Double](stateIndexes.map{_.size.toDouble})
--   val distribution:DenseVector[Double] = frequency :/ sum(frequency)
-- }
