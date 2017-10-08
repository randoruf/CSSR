{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Data.Alphabet where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V

import CSSR.Prelude

data Alphabet = Alphabet
  { idxToSym :: Vector Event
  , symToIdx :: HashMap Event Int
  } deriving (Eq, Generic, Hashable, NFData)


mkAlphabet :: HashSet Event -> Alphabet
mkAlphabet alphas = Alphabet (V.fromList list) (HM.fromList $ zip list [0..])
  where
    list :: [Event]
    list = HS.toList alphas


instance Show Alphabet where
  -- a little convoluted in the case of strings
  show (Alphabet vec _) = "Alphabet: [" ++ alphaList ++ "]"
    where
      alphaList :: String
      alphaList = intercalate "," (map show . V.toList $ vec)

