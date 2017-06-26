{- |

Pandigital numbers contain each of the digits [1..n] exactly once. For example:
21, 213, and 52314 are pandigital; 0, 11, 13, and 23 are not.

Euler problems 41 and 43 deal with pandigital numbers.

-}
module Euler.Util.Pandigital
  ( pandigitals
  , pandigitalsRev
  , pandigitalsRevOfLength
  ) where

import Euler.Prelude

import Euler.Util.Digit (unDigits)

import qualified Data.List as List

-----------------------------------------------------------------------

{- |

All pandigitals, in no particular order.

-}
pandigitals :: Integral a => [a]
pandigitals =
  permutations [0 .. 9 :: Integer] >>= \p ->
  guard (List.head p /= 0) $>
  unDigits 10 p

{- |

All pandigitals, ordered from greatest to least. The first number in this list
is 987,654,321, and the last is 1.

-}
pandigitalsRev :: Integral a => [a]
pandigitalsRev =
  foldMap pandigitalsRevOfLength [9, 8 .. (1 :: Int)]

{- |

The 1-to-/n/ pandigitals, ordered from greatest to least.

>>> pandigitalsRevOfLength 3
[321,312,231,213,132,123]

-}
pandigitalsRevOfLength :: (Integral a, Integral b) => a -> [b]
pandigitalsRevOfLength n =
  fmap (unDigits 10) . List.reverse . sort . permutations . fmap fromIntegral $
  [n, n-1 .. 1]
