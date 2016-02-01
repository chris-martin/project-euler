module Euler.Problem.Problem17 (answer) where

answer :: Int
answer = sum $ map length _1_1000 where

    _1_9   = [ "one", "two", "three", "four", "five"
             , "six", "seven", "eight", "nine" ]

    _10_19 = [ "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen"
             , "sixteen", "seventeen", "eighteen", "nineteen" ]

    _20_99 = do a <- [ "twenty", "thirty", "forty", "fifty"
                     , "sixty", "seventy", "eighty", "ninety" ]
                b <- "" : _1_9
                return (a ++ b)

    _1_99 = _1_9 ++ _10_19 ++ _20_99

    _100_999 = do a <- map (++ "hundred") _1_9
                  b <- "" : map ("and" ++) _1_99
                  return (a ++ b)

    _1_1000 = _1_99 ++ _100_999 ++ [ "onethousand" ]
