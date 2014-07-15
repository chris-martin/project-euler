module Problem3 where

import Data.Numbers.Primes (isPrime, primes)
import Data.Maybe (fromJust)
import Data.List (find)

answer :: Integer
answer = largestPrimeFactor (600851475143 :: Integer)
  where
    largestPrimeFactor n
      | isPrime n = n
      | otherwise = largestPrimeFactor $ n `div` smallestPrimeFactor n
    smallestPrimeFactor n = fromJust $ find (`divides` n) primes
    d `divides` n = n `mod` d == 0
