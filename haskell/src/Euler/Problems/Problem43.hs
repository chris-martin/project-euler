module Euler.Problems.Problem43
    ( substrings
    ) where

import Data.Digits (digits, unDigits)
import Euler.Util.List (sliding)

substrings :: Integral a => a -> [a]
-- ^ >>> substrings 1406357289
-- [406,63,635,357,572,728,289]

substrings = map (unDigits 10) . drop 1 . sliding 3 . digits 10
