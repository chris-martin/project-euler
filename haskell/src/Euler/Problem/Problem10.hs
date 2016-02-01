module Euler.Problem.Problem10 (answer) where

import Data.Numbers.Primes   ( primes )

import Euler.Util.Arithmetic ( million )

answer :: Integer
answer = sum $ takeWhile (< 2 * million) primes
