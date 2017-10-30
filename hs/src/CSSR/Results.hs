module CSSR.Results where

import CSSR.Prelude
import qualified Data.Text as T

mkMetadata :: Bool -> Text
mkMetadata conf = T.unlines $
  [ "Metadata"
  , "=========================="
  ] <> conf2unlines conf
  where
    conf2unlines :: Bool -> [Text]
    conf2unlines = undefined

mkResults :: Bool -> Bool -> Bool -> Bool -> Text
mkResults alphabet tree machine allStates = T.unlines
  [ "Results"
  , "=========================="
  , "Alphabet Size: " <> length alphabet
  , "Data Size: " <> adjustedDataSize tree
  , "Relative Entropy: " <> relativeEntropy machine
  , "Relative Entropy Rate: " <> relativeEntropyRate machine
  , "Statistical Complexity: " <> statisticalComplexity machine
  , "Entropy Rate: " <> entropyRate machine
  , "Variation: " <> machineVariation machine
  , "Number of Inferred States: " <> length allStates
  ]
  where
    adjustedDataSize = undefined
    relativeEntropy = undefined
    relativeEntropyRate = undefined
    statisticalComplexity = undefined
    entropyRate = undefined
    machineVariation = undefined
    length = undefined

mkStateDetails :: Bool -> Text
mkStateDetails allStates = undefined
--   val stateDetails: String = allStates.states
--     .view
--     .zipWithIndex
--     .map {
--       case eqClass, i =>
--         val transitions = allStates.transitionsi
--           .map{ case c, s => c -> s.flatMap{ s=> Option("State " + idxAsStr(allStates.stateMap(s)))} }
--
--         s"State ${idxAsStri}:\n" +
--           eqClass.histories.toArray.sortBy_.observed.mkString("").map{_.toString}.mkString("\n") +
--           s"""
--              |Probability Dist: ${eqClass.distribution.toString}
--              |  Frequency Dist: ${eqClass.frequency.toString}
--              |        Alphabet: ${alphabet.toString}
--              |     transitions: $transitions
--              |        Pstate: ${allStates.distribution(i)}
--              |""".stripMargin
--     }
--     .mkString"\n"
