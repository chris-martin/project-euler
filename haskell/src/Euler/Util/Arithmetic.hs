module Euler.Util.Arithmetic
    ( divides
    , million
    ) where

divides :: (Integral a, Integral b) => a -> b -> Bool
d `divides` n = fromIntegral n `mod` fromIntegral d == 0

million :: Integral a => a
million = 10 ^ 6
