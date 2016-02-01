module Euler.Problem.Problem2 (answer) where

import Euler.Util.Arithmetic ( million )
import Euler.Util.Fibonacci  ( fibs )

answer :: Integer
answer = sum $ filter even $ takeWhile (< 4 * million) fibs
