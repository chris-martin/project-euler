module Problem10 where

import Data.Numbers.Primes (primes)

answer :: Integer
answer = sum $ takeWhile (< 2*million) primes
  where million = 10^(6::Integer)
