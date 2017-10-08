{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-------------------------------------------------------------------------------
-- |
-- Module    :  Data.List.Set
-- Copyright :  (c) Sam Stites 2017
-- License   :  BSD3
-- Maintainer:  sam@sentenai.com
-- Stability :  experimental
-- Portability: non-portable
--
-- Basically a terrible implementation of Set which will work for things that
-- are of instance Eq (a looser constraint than Ord or Hashable). Nicknamed
-- "shitty-set."
-------------------------------------------------------------------------------


module Data.List.Set where

import CSSR.Prelude hiding ((<>))
import Data.List (nub, intersect)
import GHC.Exts
import Data.Semigroup (Semigroup(..))

newtype ListSet a = ListSet { unListSet :: [a] }
  deriving (Show, NFData, Generic)

instance Foldable ListSet where
  foldr f a = foldr f a . unListSet

instance Eq a => IsList (ListSet a) where
  type Item (ListSet a) = a
  fromList = ListSet . nub

instance Functor ListSet where
  fmap f = ListSet . fmap f . unListSet

instance Eq a => Eq (ListSet a) where
  (ListSet as) == (ListSet bs)
    = length as == length bs
    && all (`elem` as) bs

instance Eq a => Semigroup (ListSet a) where
  (<>) = union

instance Eq a => Monoid (ListSet a) where
  mempty = empty
  mappend = union

insert :: Eq a => a -> ListSet a -> ListSet a
insert a (ListSet as)
  | a `elem` as = ListSet as
  | otherwise   = ListSet (a:as)

member :: Eq a => a -> ListSet a -> Bool
member a (ListSet as) = a `elem` as

size :: ListSet a -> Int
size = length . unListSet

singleton :: a -> ListSet a
singleton a = ListSet [a]

empty :: ListSet a
empty = ListSet []

union :: Eq a => ListSet a -> ListSet a -> ListSet a
union (ListSet as) (ListSet bs) = ListSet (nub $ as <> bs)

intersection :: Eq a => ListSet a -> ListSet a -> ListSet a
intersection (ListSet as) (ListSet bs) = ListSet (as `intersect` bs)

