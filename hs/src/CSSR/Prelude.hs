{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
module CSSR.Prelude
  ( module X
  , groupBy
  , identity
  , unsafeHead
  , minimum
  , unsafeMinimum
  , minimumBy
  , unsafeMinimumBy
  , impossible
  , prettyDecimal
  , f'4
  , (>$>)
  , vHead
  , CSSR.Prelude.head
  , log2
  , discreteEntropy
  , GVector
  , UVector
  , LText
  , Locations
  , Idx
  , Event
  , Delim
  , DataFileContents
  ) where

import Control.Arrow       as X
import Control.Exception   as X
import Control.Monad       as X
import Control.DeepSeq     as X (NFData, deepseq)
import Data.Either         as X
import Data.Foldable       as X hiding (minimumBy, minimum)
import Data.Function       as X (on)
import Data.List           as X (intercalate, nub, (\\), sort, sortBy, delete)
import Data.Maybe          as X (catMaybes)
import Data.Monoid         as X
import Data.Hashable       as X
import Data.HashMap.Strict as X (HashMap)
import Data.HashSet        as X (HashSet)
import Data.Set            as X (Set)
import Data.Sequence       as X (Seq)
import Data.Text           as X (Text)
import Data.Vector         as X (Vector, (!))
import Data.Vector.Mutable as X (MVector)
import Debug.Trace         as X
import GHC.Exts            as X (IsList(fromList))
import GHC.Generics        as X (Generic)
import GHC.Natural         as X (Natural)
import Lens.Micro.Platform as X
import Lens.Micro.Internal as X
import Prelude             as X hiding (id, head, minimum)

import qualified Data.Vector.Generic as GV (Vector)
import qualified Data.Vector.Unboxed as UV (Vector)
import qualified Data.Text.Lazy      as LT (Text)
import qualified Data.HashMap.Strict as HM
import qualified Data.Foldable       as F
import qualified Data.Vector         as V
import qualified Prelude             as P hiding (minimum)
import Numeric

identity :: a -> a
identity = P.id

unsafeMinimum :: Ord a => Foldable t => t a -> a
unsafeMinimum = F.minimum

minimum :: Ord a => Foldable t => t a -> Maybe a
minimum as
  | null as   = Nothing
  | otherwise = Just $ unsafeMinimum as


unsafeMinimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
unsafeMinimumBy = F.minimumBy

minimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> Maybe a
minimumBy f as
  | null as   = Nothing
  | otherwise = Just $ unsafeMinimumBy f as

groupBy :: forall a b . (Hashable b, Eq b) => (a -> b) -> [a] -> [(b, [a])]
groupBy fn = HM.toList . foldr step mempty
  where
    step :: a -> X.HashMap b [a] -> X.HashMap b [a]
    step a memo = HM.alter go (fn a) memo
      where
        go :: Maybe [a] -> Maybe [a]
        go   Nothing = Just [a]
        go (Just as) = Just (a:as)

unsafeHead :: Foldable f => f a -> a
unsafeHead = P.head . toList

head :: Foldable f => f a -> Maybe a
head fs
  | null fs   = Nothing
  | otherwise = Just (unsafeHead fs)

type Locations = HashMap Idx Integer
type Idx = Integer
type Event = Text
type Delim = Text
type DataFileContents = Vector Event
type GVector = GV.Vector
type UVector = UV.Vector
type LText = LT.Text

instance Hashable x => Hashable (Vector x) where
  hashWithSalt salt = hashWithSalt salt . V.toList

impossible = error

prettyDecimal :: RealFloat a => Int -> a -> String
prettyDecimal p f = showFFloat (Just p) f ""

f'4 :: RealFloat a => a -> String
f'4 = prettyDecimal 4

(>$>) :: Monad m => (b -> m c) -> (a, b) -> m (a, c)
(>$>) f (a, b) = (a,) <$> f b

vHead :: Vector a -> Maybe a
vHead v =
  if V.null v
  then Nothing
  else Just (V.head v)


log2 :: Floating f => f -> f
log2 x = log x / log 2

discreteEntropy :: Floating f => f -> f -> f
discreteEntropy a b = a * log2 (a / b)

