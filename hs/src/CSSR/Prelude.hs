{-# LANGUAGE ScopedTypeVariables #-}
module CSSR.Prelude
  ( module X
  , groupBy
  , identity
  , Locations
  , Idx
  , Event
  , Delim
  , DataFileContents
  ) where

import Control.Arrow       as X
import Control.Exception   as X
import Control.Monad       as X
import Data.Foldable       as X
import Data.Function       as X (on)
import Data.List           as X hiding (groupBy)
import Data.Maybe          as X
import Data.Monoid         as X
import Data.Hashable       as X
import Data.HashMap.Strict as X (HashMap)
import Data.HashSet        as X (HashSet)
import Data.Sequence       as X (Seq)
import Data.Text           as X (Text)
import Data.Vector         as X (Vector, (!))
import Debug.Trace         as X
import GHC.Generics        as X (Generic)
import Lens.Micro.Platform as X
import Lens.Micro.Internal as X
import Prelude             as X hiding (id)

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V
import qualified Prelude             as P

identity :: a -> a
identity = P.id

groupBy :: forall a b . (Hashable b, Eq b) => (a -> b) -> [a] -> [(b, [a])]
groupBy fn = HM.toList . foldr step mempty
  where
    step :: a -> X.HashMap b [a] -> X.HashMap b [a]
    step a memo = HM.alter go (fn a) memo
      where
        go :: Maybe [a] -> Maybe [a]
        go   Nothing = Just [a]
        go (Just as) = Just (a:as)


type Locations = HashMap Idx Integer
type Idx = Integer
type Event = Text
type Delim = Text
type DataFileContents = Vector Event

instance Hashable x => Hashable (Vector x) where
  hashWithSalt salt = hashWithSalt salt . V.toList


