module Euler.Problems.Problem19
  ( Date(..)
  , datesFrom1900
  , answer
  ) where

import Euler.Prelude

import Euler.Util.Date (monthLength)

------------------------------------------------------

-- | The number of Sundays that fell on the first of the month
-- during the twentieth century (1 Jan 1901 to 31 Dec 2000).
answer :: Integer
answer =
  datesFrom1900 & filter isMatchingDate & length & fromIntegral

data Date = Date
    { dateYear    :: Int -- ^ Anno Domini
    , dateMonth   :: Int -- ^ 1 (january) to 12 (december)
    , dateDay     :: Int -- ^ 1 to 31
    , dateWeekday :: Int -- ^ 1 (money) to 7 (sunday)
    }

-- | All dates starting from Jan 1, 1900.
datesFrom1900 :: [Date]
datesFrom1900 =
    zipWith f dates weekdays
  where

    f (year, month, day) weekday =
      Date { dateYear    = year
           , dateMonth   = month
           , dateDay     = day
           , dateWeekday = weekday
           }

    dates =
      [1900 .. 2000] >>= \year ->
      [1 .. 12] >>= \month ->
      [1 .. monthLength year month] <&> \day ->
      (year, month, day)

    weekdays = cycle [1 .. 7]

isMatchingDate :: Date -> Bool
isMatchingDate d =
  dateYear d /= 1900 &&
  dateDay d == 1 &&
  dateWeekday d == 7
