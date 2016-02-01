module Euler.Problem.Problem2 (answer) where

import Euler.Util.Fibonacci (fibs)

answer :: Integer
answer = sum $ filter even $ takeWhile (< bound) fibs where
    bound = 4 * million
    million = 10 ^ 6
