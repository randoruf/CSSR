module CSSR.Results where

import Protolude
import System.Directory (getCurrentDirectory)

import Data.CSSR.Alphabet
import Data.CSSR.State
import qualified Prelude as P
import qualified Data.Text as T

-- | All top-level CSSR metadata (FIXME: should be used in the CLI)
data Metadata = Metadata
  { workingDirectory :: FilePath
  , alphabetFile :: FilePath
  , dataFile :: FilePath
  , maxHistoryLen :: Int
  , multiline :: Bool
  , delimiter :: Text
  , significance :: Double
  , useChiSquared :: Bool
  , stateLabels :: Bool
  , out :: Bool
  , debug :: Bool
  }

instance P.Show Metadata where
  show m = P.unlines
    [ "Metadata"
    , "=========================="
    , "Current Working Directory: " <> workingDirectory m
    , "Alphabet file: " <> alphabetFile m
    , "Alphabet delimiter: " <> T.unpack (delimiter m)
    , "Data file: " <> dataFile m
    , "History Length: " <> show (maxHistoryLen m)
    , "Multi-line mode: " <> show (multiline m)
    , "Significance level: " <> show (significance m)
    , "Chi-squared test used: " <> show (useChiSquared m)
    ]

-- | Smart constructor for all top-level CSSR metadata
mkMetadata
  :: FilePath -- ^ alphabetFile
  -> FilePath -- ^ dataFile
  -> Int      -- ^ max history length
  -> Text     -- ^ delimiter
  -> Double   -- ^ significance level
  -> Bool     -- ^ label states with alphabet?
  -> Bool     -- ^ report to stdout?
  -> Bool     -- ^ verbosely log for debugging?
  -> IO Metadata
mkMetadata a d len del sig label o dbug = do
  cwd <- getCurrentDirectory
  pure $ Metadata
    { workingDirectory = cwd
    , alphabetFile = a
    , dataFile = d
    , maxHistoryLen = len
    , multiline = False
    , delimiter = del
    , significance = sig
    , useChiSquared = False
    , stateLabels = label
    , out = o
    , debug = dbug
    }

-- | Reported results from CSSR
data Results = Results
  { alphabetSize :: Word
  , dataSize :: Word
  , relativeEntropy :: Double
  , relativeEntropyRate :: Double
  , statisticalComplexity :: Double
  , entropyRate :: Double
  , variation :: Double
  , inferredStates :: Word
  }

instance Show Results where
  show r = P.unlines
    [ "Results"
    , "=========================="
    , "Alphabet Size: " <> show (alphabetSize r)
    , "Data Size: " <> show (dataSize r)
    , "Relative Entropy: " <> show (relativeEntropy r)
    , "Relative Entropy Rate: " <> show (relativeEntropyRate r)
    , "Statistical Complexity: " <> show (statisticalComplexity r)
    , "Entropy Rate: " <> show (entropyRate r)
    , "Variation: " <> show (variation r)
    , "Number of Inferred States: " <> show (inferredStates r)
    ]

-- | Smart constructor for CSSR results
mkResults :: Alphabet -> Bool -> Bool -> AllStates -> Results
mkResults alphabet tree machine allStates = undefined

mkStateDetails :: AllStates -> Text
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
