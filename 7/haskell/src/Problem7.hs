module Problem7 where

import Data.Numbers.Primes (primes)

answer :: Integer
answer = primes !! 10000
