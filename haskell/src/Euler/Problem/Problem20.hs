module Euler.Problem.Problem20 (answer) where

import Euler.Util.Arithmetic ( factorial )
import Euler.Util.Digit      ( intDigits )

answer :: Integer
answer = sum $ intDigits $ factorial 100
