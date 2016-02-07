module Euler.Util.Amicable (amicableNumbers) where

import qualified Data.Map as Map
import Euler.Util.Prime (properDivisorsOfPrimeProduct, factorizations)

amicableNumbers :: Integer -> [Integer]
amicableNumbers max = filter isAmicable [2..max] where

    -- "Let d(n) be defined as the sum of proper divisors of n"
    divisorSums = fmap (sum . properDivisorsOfPrimeProduct) $ factorizations max
    d  = (divisorSums Map.!)
    d' = (`Map.lookup` divisorSums)

    -- "If d(a) = b and d(b) = a, where a ≠ b, then a and b are an
    -- amicable pair and each of a and b are called amicable numbers."
    isAmicable a = let b = d a in b /= a && d' b == Just a
