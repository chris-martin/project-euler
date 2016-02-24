module Euler.Problems.Problem31 (answer) where

answer :: Integer
answer = count []

count :: Integral a => [a] -> a
count base | p == target                         = 1
           | p >  target                         = 0
           | length base == length denominations = 0
           | otherwise                           = recurse
  where
    p = pence base
    recurse = (sum . (map (\n -> count (base ++ [n]))))
              [0 .. (target - p) `div` (denominations !! (length base))]

target :: Integral a => a
target = 200

pence :: Integral a => [a] -> a
pence = sum . (zipWith (*) denominations)

denominations :: Integral a => [a]
denominations = [1, 2, 5, 10, 20, 50, 100, 200]
