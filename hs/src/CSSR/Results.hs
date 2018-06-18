module CSSR.Results where

import Protolude hiding (State)
import Data.Vector (Vector)
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import Control.Arrow ((&&&))
import System.Directory (getCurrentDirectory)
import Data.Maybe (fromJust)
import Lens.Micro.Platform

import Data.CSSR.Alphabet
import Data.CSSR.State hiding (transitions)
import qualified Prelude as P
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.CSSR.State as CSSR
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

data Detail = Detail
  { detailIx :: StateIx
  , observations :: HashSet (Vector Event)
  , probDist :: HashMap Event Double
  , freqDist :: HashMap Event Integer
  , transitions :: Vector (State, HashMap Event (Maybe StateIx))
  , probState :: Double
  }

instance Show Detail where
  show d = T.unpack . T.unlines $
    [ show (detailIx d) <> ":" ]
    <> map show (toList (observations d)) <>
    [ "Probability Dist: " <> renderDist show (probDist d)
    , "  Frequency Dist: " <> renderDist show (freqDist d)
    , "     transitions: " <> show (transitions d)
    , "        P(state): " <> show (probState d)
    ]
   where
    renderDist :: (n -> Text) -> HashMap Event n -> Text
    renderDist repr = T.intercalate "," . map repr . toList


mkDetail :: Alphabet -> Vector State -> State -> Detail
mkDetail a states s =
  Detail
    (getIx' s)
    (HS.map (Cond.obs . Cond.body) $ terminalHists s)
    (distributionsLookup a states s)
    (frequencyLookup a states s)
    ((identity &&& HM.map (maybe Nothing (getIx states)) . toTs) <$> states)
    (distributionLookup a states s)
 where
  getIx' :: State -> StateIx
  getIx' = fromJust (panic "all states accounted for in results") (getIx states)

  terminalHists :: State -> HashSet Cond.Leaf
  terminalHists = view (Loop.bodyL . _Right . Loop.historiesL) . terminal

  toTs :: State -> HashMap Event (Maybe State)
  toTs = allTransitionsLookup a states


mkStateDetails :: Alphabet -> Vector State -> Text
mkStateDetails a states
  = T.unlines
  $ map (show . mkDetail a states)
  $ toList states

--  where
--   terminalHists :: State -> HashSet Cond.Leaf
--   terminalHists = view (Loop.bodyL . _Right . Loop.historiesL) . terminal

--   stateFreq :: State -> Text
--   stateFreq = renderDist show . frequencyLookup a states


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

