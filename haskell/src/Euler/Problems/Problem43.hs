module Euler.Problems.Problem43
  ( answer
  , substrings
  , predicate
  ) where

import Euler.Prelude

import Data.List (sum, and, filter, zipWith, drop)

import Euler.Util.Arithmetic (divides)
import Euler.Util.Digit (digits, unDigits)
import Euler.Util.List (sliding)
import Euler.Util.Pandigital (pandigitals)
import Euler.Util.Prime (primes)

answer :: Integer
answer =
  sum $ filter predicate pandigitals

predicate :: Integer -> Bool
predicate i =
  and $ zipWith divides (primes :: [Integer]) (substrings i)

{- |

Three-digit substrings of the decimal representation of the input.

>>> substrings 1406357289
[406,63,635,357,572,728,289]

-}
substrings :: Integral a => a -> [a]
substrings =
  digits 10 >>>
  sliding 3 >>>
  drop 1 >>>
  fmap (unDigits (10 :: Integer))
