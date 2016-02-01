module Euler.Util.Fibonacci
    ( fibs
    ) where

fibs :: Integral a => [a]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
