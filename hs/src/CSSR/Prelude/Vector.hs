module CSSR.Prelude.Vector
  ( head
  , last
  , tail
  , init
  , module X
  ) where

import CSSR.Prelude (saferBy, Vector, Maybe)
import Data.Vector as X hiding (head, tail, last, init)

import qualified Data.Vector as V

head :: Vector a -> Maybe a
head = V.head `saferBy` V.null

last :: Vector a -> Maybe a
last = V.last `saferBy` V.null

tail :: Vector a -> Maybe (Vector a)
tail = V.tail `saferBy` V.null

init :: Vector a -> Maybe (Vector a)
init = V.init `saferBy` V.null

