module Euler.Util.Map (keyWithMaxValue) where

import Data.Foldable ( maximumBy )
import Data.Map      ( Map )
import Data.Ord      ( comparing )

import qualified Data.Map as Map

keyWithMaxValue :: (Ord a) => Map k a -> k
keyWithMaxValue = fst . (maximumBy $ comparing snd) . Map.toList
