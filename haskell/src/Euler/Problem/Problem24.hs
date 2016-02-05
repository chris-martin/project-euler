module Euler.Problem.Problem24 (answer) where

import Data.List ( permutations, sort )

import Euler.Util.Arithmetic ( million )

answer :: String
answer = (sort $ permutations ['0'..'9']) !! (million - 1)
