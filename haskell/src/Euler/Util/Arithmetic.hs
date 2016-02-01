module Euler.Util.Arithmetic
    ( divides
    ) where

divides :: (Integral a, Integral b) => a -> b -> Bool
d `divides` n = fromIntegral n `mod` fromIntegral d == 0
