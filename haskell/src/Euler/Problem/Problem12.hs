module Euler.Problem.Problem12 (answer, triangles) where

import Euler.Util.Prime (countDivisors)

answer :: Integer
answer = head $ filter (\n -> countDivisors n > 500) $ triangles

triangles :: [Integer]
triangles = scanl1 (+) [1..]
