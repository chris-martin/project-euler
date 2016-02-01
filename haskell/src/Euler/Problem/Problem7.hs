module Euler.Problem.Problem7 (answer) where

import Data.Numbers.Primes (primes)

answer :: Integer
answer = primes !! 10000
