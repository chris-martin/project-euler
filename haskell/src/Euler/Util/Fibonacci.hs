module Euler.Util.Fibonacci (fibs) where

fibs :: Integral a => [a]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
-- ^ >>> take 13 fibs
-- [0,1,1,2,3,5,8,13,21,34,55,89,144]
