module CSSR.Prelude.Test
  ( module X
  , str2Event
  )
  where

import CSSR.Prelude as X

import Data.Maybe as X
import Test.Hspec as X
import Test.Hspec.QuickCheck as X
import qualified Data.Text as T
import qualified Data.Vector as V



str2Event :: Event -> Vector Event
str2Event = V.fromList . fmap T.singleton . T.unpack


