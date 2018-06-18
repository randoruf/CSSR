module CSSR.Results where

import Protolude hiding (State)
import Data.Vector (Vector)
import Data.HashMap.Strict (HashMap)
import Control.Arrow ((&&&))
import System.Directory (getCurrentDirectory)
import Data.Maybe (fromJust)
import Lens.Micro.Platform

import Data.CSSR.Alphabet
import Data.CSSR.State
import qualified Prelude as P
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Tree.Conditional as Cond
import Data.Tree.Looping as Loop

import CSSR.Results.GraphViz (getIx, StateIx)

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

mkStateDetails :: Alphabet -> Vector State -> Text
mkStateDetails a states
  = T.unlines
  $ map oneStateDetails
  $ toList states

 where
  getIx' :: State -> Maybe StateIx
  getIx' = fromJust (panic "all states accounted for in results") (getIx states)

  terminalHists :: State -> HS.HashSet Cond.Leaf
  terminalHists = view (Loop.bodyL . _Right . Loop.historiesL) . terminal

  oneStateDetails :: State -> Text
  oneStateDetails s = T.unlines $
    [ show (getIx' s) <> ":" ]
    <> toList (HS.map (show . Cond.obs . Cond.body) $ terminalHists s) <>
    [ "Probability Dist: " <> stateDist s
    , "  Frequency Dist: " <> stateFreq s
    , "        Alphabet: " <> alphaList a
    , "     transitions: " <> show ts
    , "        P(state): " <> stateProb s
    ]

  ts :: Vector (State, HashMap Event (Maybe StateIx))
  ts = (identity &&& HM.map (maybe Nothing (getIx states)) . toTs) <$> states

  renderDist :: (n -> Text) -> HashMap Event n -> Text
  renderDist repr = T.intercalate "," . map repr . toList

  toTs :: State -> HashMap Event (Maybe State)
  toTs = allTransitionsLookup a states

  stateFreq :: State -> Text
  stateFreq = renderDist show . frequencyLookup a states

  stateDist :: State -> Text
  stateDist = renderDist show . distributionsLookup a states

  stateProb :: State -> Text
  stateProb = show . distributionLookup a states


