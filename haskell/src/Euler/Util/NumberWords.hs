module Euler.Util.NumberWords (word) where

word :: Int -> String

word  1 = "one"
word  2 = "two"
word  3 = "three"
word  4 = "four"
word  5 = "five"
word  6 = "six"
word  7 = "seven"
word  8 = "eight"
word  9 = "nine"
word 10 = "ten"
word 11 = "eleven"
word 12 = "twelve"
word 13 = "thirteen"
word 14 = "fourteen"
word 15 = "fifteen"
word 16 = "sixteen"
word 17 = "seventeen"
word 18 = "eighteen"
word 19 = "nineteen"
word 20 = "twenty"
word 30 = "thirty"
word 40 = "forty"
word 50 = "fifty"
word 60 = "sixty"
word 70 = "seventy"
word 80 = "eighty"
word 90 = "ninety"

word i | i < 100 =
  let (a, b) = quotRem i 10
  in (word (10 * a)) ++ (word b)

word i | i < 1000 =
  case quotRem i 100 of
    (a, 0) -> (word a) ++ "hundred"
    (a, b) -> (word a) ++ "hundredand" ++ (word b)

word 1000 = "onethousand"
