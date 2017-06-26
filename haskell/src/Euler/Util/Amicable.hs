{- |

Let /d(n)/ be defined as the sum of proper divisors of /n/.

If /d(a) = b/ and /d(b) = a/, where /a ≠ b/, then /a/ and /b/ are an amicable
pair and each of /a/ and /b/ are called amicable numbers.

Amicable numbers are defined in Euler problem 21.

 -}
module Euler.Util.Amicable
  ( amicableNumbers
  ) where

import Euler.Prelude hiding (max)

import qualified Data.List as List
import qualified Data.Map as Map

import Euler.Util.Prime (factorizations, properDivisorsOfPrimeProduct)

--------------------------------------------------------------------------------

{- |

@'amicableNumbers' n@ gives all of the amicable numbers on the inclusive
range [2, /n/], in ascending order.

-}
amicableNumbers :: Integer -> [Integer]
amicableNumbers max = List.filter isAmicable [2..max]
  where
    divisorSums = (List.sum . properDivisorsOfPrimeProduct) <$> factorizations max
    d  = (divisorSums Map.!)
    d' = (`Map.lookup` divisorSums)

    isAmicable a = let b = d a in b /= a && d' b == Just a
