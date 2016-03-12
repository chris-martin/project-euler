module Euler.Problems.Problem43
    ( answer
    , substrings
    ) where

import Euler.Util.Arithmetic (divides)
import Euler.Util.Digit      (digits, unDigits)
import Euler.Util.List       (sliding)
import Euler.Util.Pandigital (pandigitals)
import Euler.Util.Prime      (primes)

---------------------------------------------------------

answer :: Integer

predicate :: Integer -> Bool

substrings :: Integral a => a -> [a]
-- ^ >>> substrings 1406357289
-- [406,63,635,357,572,728,289]

---------------------------------------------------------

answer = sum (filter predicate pandigitals)

predicate i = and (zipWith divides
                    (primes :: [Integer])
                    (substrings i))

substrings = map (unDigits 10) . drop 1 . sliding 3 . digits 10
