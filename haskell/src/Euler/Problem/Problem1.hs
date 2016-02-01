module Euler.Problem.Problem1 (answer) where

import Euler.Util.Arithmetic (divides)

answer :: Integer
answer = sum $ filter f [1..999]
    where f n = any (`divides` n) [3, 5]
