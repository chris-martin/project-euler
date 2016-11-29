module Euler.Problems.Problem33
    ( answer, specialFractions, isCurious ) where

import Euler.Prelude

import Euler.Util.Digit (digits)

-------------------------------------------------------------

answer :: (Integral a) => a

specialFractions :: (Integral a) => [(a, a)]

isCurious :: (Integral a) => (a, a) -> Bool

-------------------------------------------------------------

answer = (denominator . product) (map (uncurry (%)) specialFractions)

specialFractions = filter isCurious [ (c, d) | c <- [10    .. 99]
                                             , d <- [c + 1 .. 99] ]

isCurious (c, d) = any f [ (a, b) | a <- permutations (digits 10 c)
                                  , b <- permutations (digits 10 d) ]
  where
    f ([a0, a1], [b0, b1]) = a0 /= 0   &&
                             b1 /= 0   &&
                             a0 == b0  &&
                             c % d == a1 % b1
    f _ = undefined
