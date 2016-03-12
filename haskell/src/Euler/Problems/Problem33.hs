module Euler.Problems.Problem33 (answer) where

import Data.Digits (digits)
import Data.List   (permutations)
import Data.Ratio  (denominator, (%))

answer :: Integer
answer = (denominator . product) (map (uncurry (%)) specialFractions)

specialFractions :: Integral a => [(a, a)]
specialFractions = filter isCurious $ do c <- [10    .. 99]
                                         d <- [c + 1 .. 99]
                                         return (c, d)

isCurious :: Integral a => (a, a) -> Bool
isCurious (c, d) = any f $ do a <- permutations (digits 10 c)
                              b <- permutations (digits 10 d)
                              return (a, b)
  where
    f ([a0, a1], [b0, b1]) = a0 /= 0   &&
                             b1 /= 0   &&
                             a0 == b0  &&
                             c % d == a1 % b1
    f _ = undefined
