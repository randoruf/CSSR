-------------------------------------------------------------------------------
-- |
-- Module    :  Data.CSSR.Alphabet
-- Copyright :  (c) Sam Stites 2017
-- License   :  BSD3
-- Maintainer:  sam@stites.io
-- Stability :  experimental
-- Portability: non-portable
--
-- Representations of Symbols that CSSR works on and how to build them.
-------------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Data.CSSR.Alphabet where

import Protolude
import qualified Prelude as P

import Data.Vector (Vector)
import Data.Vector.Instances ()
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V

type Event = Text
type Symbol = Event

data Alphabet = Alphabet
  { idxToSym :: Vector Event
  , symToIdx :: HashMap Event Int
  } deriving (Eq, Generic, Hashable, NFData)


mkAlphabet :: HashSet Event -> Alphabet
mkAlphabet alphas = Alphabet (V.fromList list) (HM.fromList $ zip list [0..])
  where
    list :: [Event]
    list = HS.toList alphas

size :: Alphabet -> Int
size = V.length . idxToSym

instance Show Alphabet where
  -- a little convoluted in the case of strings
  show (Alphabet vec _) = "Alphabet: [" ++ alphaList ++ "]"
    where
      alphaList = intercalate "," (map show . V.toList $ vec)

