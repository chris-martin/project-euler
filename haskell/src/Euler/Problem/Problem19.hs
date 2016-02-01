{-# LANGUAGE NamedFieldPuns #-}

module Euler.Problem.Problem19 (answer) where

import Euler.Util.Date (monthLength)

data Date = Date { year :: Int, month :: Int, day :: Int }

answer :: Int
answer = length $ filter isMatchingDate datesWithWeekday where

    dates = [ Date { year, month, day }
            | year  <- [1900 .. 2000]
            , month <- [1 .. 12]
            , day   <- [1 .. monthLength year month]
            ]

    datesWithWeekday = zip dates $ cycle [1 .. 7]

    isMatchingDate (Date {year, day}, weekday) =
        year /= 1900 && day == 1 && weekday == 7
