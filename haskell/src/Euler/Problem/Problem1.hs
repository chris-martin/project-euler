module Euler.Problem.Problem1 (answer) where

import Euler.Util.Arithmetic (divides)

answer :: Integer
answer = sum multiples
  where
    multiples = filter isMultiple [1..999]
    isMultiple n = any (`divides` n) [3, 5]
