package com.typeclassified.hmm.cssr.state

import breeze.linalg.{sum, DenseVector}
import com.typeclassified.hmm.cssr.measure._
import com.typeclassified.hmm.cssr.parse.{Alphabet, Leaf, Tree}
import com.typeclassified.hmm.cssr.measure.{InferProbabilities => I}
import com.typeclassified.hmm.cssr.state.{Machine => M}
import com.typeclassified.hmm.cssr.state.Machine.{StateToAllHistories, StateTransitionMap}
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Machine {
  protected val logger = Logger(LoggerFactory.getLogger(Machine.getClass))

  type StateTransitionMap = Array[Map[Char, Option[Int]]]

  type StateToAllHistories = Map[Option[EquivalenceClass], Array[Leaf]]

  def allHistoriesByState (tree: Tree, states:Array[EquivalenceClass]):StateToAllHistories = {
    tree.collectLeaves()
      .foldLeft(mutable.Map[Option[EquivalenceClass], ArrayBuffer[Leaf]]()){
        (map, leaf) => {
          val eq = leaf.currentEquivalenceClass
          if (map.keySet.contains(Option(eq))) map(Option(eq)) += leaf
          else if (!states.contains(eq)) map(None) += leaf
          else map(Option(eq)) = ArrayBuffer(leaf)
          map
        }
      }.toMap.mapValues(_.toArray)
  }

  def findNthSetTransitions(states:Array[EquivalenceClass], maxDepth: Int, alphabet: Alphabet, fullStates:StateToAllHistories)
  :StateTransitionMap = {
    val transitions = states.map {
      equivalenceClass => {
        val startHistories = fullStates(Option(equivalenceClass)).filter(_.observed.length == maxDepth-1)
        val endHistories = startHistories.flatMap(_.children)
        endHistories.groupBy(_.observation)
          .mapValues[Option[Int]] {
          nextHistories => {
            val validNextStates = nextHistories.map(_.currentEquivalenceClass).filter(states.contains(_)).toSet
            if (validNextStates.size != 1) {
              None
            } else {
              Option(states.indexOf(validNextStates.head))
            }
          } }
      } }

    ensureFullTransitionMap(transitions, alphabet.raw)
  }

  protected def ensureFullTransitionMap(transitionMap:StateTransitionMap, symbolsToFill: Array[Char]):StateTransitionMap = {
    transitionMap.map {
      symbolsToFill.foldRight(_) {
        (alphabetChar, tMap) => if (tMap.keySet.contains(alphabetChar)) tMap else tMap + (alphabetChar -> None)
      }
    }
  }
}

class Machine (equivalenceClasses: ListBuffer[EquivalenceClass], tree:Tree) {
  // initialization
  val states:Array[EquivalenceClass]           = equivalenceClasses.toArray
  val stateIndexes:Array[Set[Int]]             = states.map{_.histories.flatMap{_.locations.keySet}}
  val statePaths:Array[Array[String]]          = states.map{_.histories.map{_.observed}.toArray}

  val frequency:DenseVector[Double]            = new DenseVector[Double](stateIndexes.map{_.size.toDouble})
  val distribution:DenseVector[Double]         = frequency :/ sum(frequency)

  val fullStates:StateToAllHistories           = M.allHistoriesByState(tree, states)
  val transitionsByStateIdx:StateTransitionMap = M.findNthSetTransitions(states, tree.maxLength, tree.alphabet, fullStates)

  // Requires context of the machine itself -> not ideal, but logical
  val inferredDistribution:I.InferredDistribution = I.inferredDistribution(tree, tree.maxLength, this)

  val variation:Double             = Variation.variation(inferredDistribution, tree.adjustedDataSize)
  val entropyRate:Double           = EntropyRate.entropyRate(this)
  val relativeEntropy:Double       = RelativeEntropy.relativeEntropy(inferredDistribution, tree.adjustedDataSize)
  val relativeEntropyRate:Double   = RelativeEntropyRate.relativeEntropyRate(inferredDistribution, tree, this)
  val statisticalComplexity:Double = StatisticalComplexity.cMu(distribution)
}

