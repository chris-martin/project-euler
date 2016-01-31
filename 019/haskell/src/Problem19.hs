{-# LANGUAGE NamedFieldPuns #-}

module Problem19 (answer, monthLength, yearLength) where

------------------------------------------------------
-- Month length
------------------------------------------------------

monthLength :: Int -> Int -> Int
monthLength y 2 = if leap y then 29 else 28 -- february
--                 jan feb mar apr may jun jul aug sep oct nov dec
monthLength _ m = [31, 0,  31, 30, 31, 30, 31, 31, 30, 31, 30, 31] !! (m - 1)

yearLength :: Int -> Int
yearLength year = sum $ [ monthLength year month | month <- [1..12] ]

leap :: Int -> Bool
leap year | year `mod` 400 == 0 = True
          | year `mod` 100 == 0 = False
          | year `mod`   4 == 0 = True
          | otherwise       = False

------------------------------------------------------
-- Answer
------------------------------------------------------

data Date = Date { year :: Int, month :: Int, day :: Int }

answer :: Int
answer = length $ filter isMatchingDate datesWithWeekday where

    dates = [ Date { year, month, day }
            | year  <- [1900..2000]
            , month <- [1..12]
            , day   <- [1.. monthLength year month]
            ]

    datesWithWeekday = zip dates $ cycle [1..7]

    isMatchingDate (Date {year, day}, weekday) =
        year /= 1900 && day == 1 && weekday == 7
