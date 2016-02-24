module Euler.Problems.Problem43
    ( substrings
    ) where

import Data.Digits (digits, unDigits)
import Euler.Util.List (sliding)

substrings :: Integral a => a -> [a]
substrings = (map (unDigits 10)) . (drop 1) . (sliding 3) . (digits 10)
