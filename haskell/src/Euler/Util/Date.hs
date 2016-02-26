module Euler.Util.Date
    ( monthLength
    , yearLength
    , leap) where

-- | @'monthLength' y m@ is the number of days in month @m@ of year @y@.
monthLength :: Int -> Int -> Int

-- | The number of days in a year AD.
yearLength :: Int -> Int

-- | Whether a year AD is a leap year. Leap years contain an extra day
-- in February.
leap :: Int -> Bool

-----------------------------------------------------------------------

monthLength y 2 = if leap y then 29 else 28 -- february
--                 jan feb mar apr may jun jul aug sep oct nov dec
monthLength _ m = [31, 0,  31, 30, 31, 30, 31, 31, 30, 31, 30, 31] !! (m - 1)

yearLength year = sum $ [ monthLength year month | month <- [1 .. 12] ]

leap year | year `mod` 400 == 0 = True
          | year `mod` 100 == 0 = False
          | year `mod`   4 == 0 = True
          | otherwise           = False
