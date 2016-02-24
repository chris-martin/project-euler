-- | Let /d(n)/ be defined as the sum of proper divisors of /n/.
--
-- If /d(a) = b/ and /d(b) = a/, where /a ≠ b/, then /a/ and /b/ are an
-- amicable pair and each of /a/ and /b/ are called amicable numbers.

module Euler.Util.Amicable (amicableNumbers) where

import qualified Data.Map as Map
import Euler.Util.Prime (properDivisorsOfPrimeProduct, factorizations)

-- | @'amicableNumbers' n@ gives all of the amicable numbers on the inclusive
-- range [2, /n/], in ascending order.
amicableNumbers :: Integer -> [Integer]
amicableNumbers max = filter isAmicable [2..max] where

    divisorSums = fmap (sum . properDivisorsOfPrimeProduct) $ factorizations max
    d  = (divisorSums Map.!)
    d' = (`Map.lookup` divisorSums)

    isAmicable a = let b = d a in b /= a && d' b == Just a
