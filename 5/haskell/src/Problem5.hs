module Problem5 where

import Data.Numbers.Primes (primes)

answer :: Integer
answer = product factors
  where

    -- greatest powers of primes within the bound
    factors = map powerUp $ takeWhile (<= bound) primes

    powerUp n = last $ takeWhile (<= bound) $ powersOf n
    powersOf n = map (n^) [ 1 :: Int .. ]
    bound = 20
