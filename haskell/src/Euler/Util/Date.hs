module Euler.Util.Date (monthLength, yearLength, leap) where

monthLength :: Int -> Int -> Int
yearLength  :: Int        -> Int
leap        :: Int        -> Bool

-----------------------------------------------------------------------

monthLength y 2 = if leap y then 29 else 28 -- february
--                 jan feb mar apr may jun jul aug sep oct nov dec
monthLength _ m = [31, 0,  31, 30, 31, 30, 31, 31, 30, 31, 30, 31] !! (m - 1)

yearLength year = sum $ [ monthLength year month | month <- [1 .. 12] ]

leap year | year `mod` 400 == 0 = True
          | year `mod` 100 == 0 = False
          | year `mod`   4 == 0 = True
          | otherwise       = False
