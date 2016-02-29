module Euler.Problems.Problem19
    ( answer
    ) where

import Euler.Util.Date (monthLength)

------------------------------------------------------

answer :: Integer

data Date = Date
    { dateYear    :: Int -- ^ Anno Domini
    , dateMonth   :: Int -- ^ 1 (january) to 12 (december)
    , dateDay     :: Int -- ^ 1 to 31
    , dateWeekday :: Int -- ^ 1 (money) to 7 (sunday)
    }

datesFrom1900 :: [Date]

isMatchingDate :: Date -> Bool

------------------------------------------------------

answer = (fromIntegral . length . filter isMatchingDate) datesFrom1900

datesFrom1900 = zipWith f dates weekdays
  where

    f (year, month, day) weekday =
      Date { dateYear    = year
           , dateMonth   = month
           , dateDay     = day
           , dateWeekday = weekday
           }

    dates = do year  <- [1900 .. 2000]
               month <- [1 .. 12]
               day   <- [1 .. monthLength year month]
               return (year, month, day)

    weekdays = cycle [1 .. 7]

isMatchingDate d =
    (dateYear    d) /= 1900 &&
    (dateDay     d) == 1    &&
    (dateWeekday d) == 7
