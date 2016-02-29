-- | Pandigital numbers contain each of the digits [1..n] exactly once. For
-- example: 21, 213, and 52314 are pandigital; 0, 11, 13, and 23 are not.
--
-- Euler problems 41 and 43 deal with pandigital numbers.

module Euler.Util.Pandigital
    ( pandigitals
    , pandigitalsRev
    , pandigitalsRevOfLength
    ) where

import Control.Monad ( guard )
import Data.Digits   ( unDigits )
import Data.List     ( permutations, sort )

-----------------------------------------------------------------------

pandigitals :: Integral a => [a]
-- ^ All pandigitals, in no particular order.

pandigitalsRev :: Integral a => [a]
-- ^ All pandigitals, ordered from greatest to least.
-- The first number in this list is 987,654,321, and the last is 1.

pandigitalsRevOfLength :: (Integral a, Integral b) => a -> [b]
-- ^ The 1-to-/n/ pandigitals, ordered from greatest to least.
--
-- >>> pandigitalsRevOfLength 3
-- [321,312,231,213,132,123]

-----------------------------------------------------------------------

pandigitals = do
    p <- permutations [0..9]
    guard (head p /= 0)
    return (unDigits 10 p)

pandigitalsRev = concatMap pandigitalsRevOfLength [9, 8 .. (1 :: Int)]

pandigitalsRevOfLength n =
    ((map (unDigits 10)) . reverse . sort . permutations . (map fromIntegral))
    [n, n-1 .. 1]
