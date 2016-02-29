module Euler.Util.Map (keyWithMaxValue) where

import Data.Foldable ( maximumBy )
import Data.Map      ( Map )
import Data.Ord      ( comparing )

import qualified Data.Map as Map

keyWithMaxValue :: (Ord a) => Map k a -> k
-- ^ >>> keyWithMaxValue $ Map.fromList [('a', 4), ('b', 12), ('c', 6)]
-- 'b'
keyWithMaxValue = fst . (maximumBy $ comparing snd) . Map.toList
