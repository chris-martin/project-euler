module Euler.Problem.Problem16 (answer) where

import Euler.Util.Digit ( intDigits )

answer :: Integer
answer = sum $ intDigits $ 2 ^ 1000
