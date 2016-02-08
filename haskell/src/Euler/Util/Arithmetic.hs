module Euler.Util.Arithmetic
    (
    -- * Functions
      divides
    , factorial, factorials
    , square

    -- * Constants
    , million
    ) where

divides    :: (Integral a, Integral b) => a -> b -> Bool

factorial  :: Integral a => a -> Integer
factorials :: [Integer]

square     :: Num a => a -> a

million    :: Integral a => a

---------------------------------------------------------

d `divides` n = fromIntegral n `mod` fromIntegral d == 0

factorial n = product [1 .. fromIntegral n]

factorials = 1 : (scanl1 (*) [1..])

square x = x * x

million = 10 ^ 6
