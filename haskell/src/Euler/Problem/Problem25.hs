module Euler.Problem.Problem25 (answer) where

import Data.List            ( findIndex )
import Data.Maybe           ( fromJust )

import Euler.Util.Fibonacci ( fibs )

answer :: Int
answer = fromJust $ findIndex (>= x) fibs where x = 10 ^ 999
