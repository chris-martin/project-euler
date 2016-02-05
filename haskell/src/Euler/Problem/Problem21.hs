module Euler.Problem.Problem21 (answer) where

import qualified Data.Map as Map

import Euler.Util.Prime ( properDivisorsOfPrimeProduct, factorizations )

-- "the sum of all the amicable numbers under 10000"
answer :: Integer
answer = sum $ amicableNumbersUnder 10000

amicableNumbersUnder :: Integer -> [Integer]
amicableNumbersUnder bound = filter isAmicable [2..max] where

    max = bound - 1

    -- "Let d(n) be defined as the sum of proper divisors of n"
    divisorSums = fmap (sum . properDivisorsOfPrimeProduct) (factorizations max)
    d  = (divisorSums Map.!)
    d' = (`Map.lookup` divisorSums)

    -- "If d(a) = b and d(b) = a, where a ≠ b, then a and b are an
    -- amicable pair and each of a and b are called amicable numbers."
    isAmicable a = let b = d a in b /= a && d' b == Just a
