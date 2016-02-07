module Euler.Util.Arithmetic
    (
    -- * Functions
      divides
    , factorial
    , square

    -- * Constants
    , million
    ) where

divides :: (Integral a, Integral b) => a -> b -> Bool
d `divides` n = fromIntegral n `mod` fromIntegral d == 0

million :: Integral a => a
million = 10 ^ 6

factorial :: Integral a => a -> Integer
factorial n = product [1 .. fromIntegral n]

square :: Num a => a -> a
square x = x * x
