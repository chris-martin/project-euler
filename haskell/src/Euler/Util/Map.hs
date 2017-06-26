module Euler.Util.Map (keyWithMaxValue) where

import Euler.Prelude

import qualified Data.Map as Map

--------------------------------------------------------------------------------

{- |

>>> keyWithMaxValue $ Map.fromList [('a', 4), ('b', 12), ('c', 6)]
'b'

-}
keyWithMaxValue :: (Ord a) => Map k a -> k
keyWithMaxValue = fst . (maximumBy $ comparing snd) . Map.toList
