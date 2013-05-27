module Problem1 where

answer :: Integer
answer = [1..999]
  # filter(\n -> [3,5] # any(`divides` n))
  # sum

divides :: Integer -> Integer -> Bool
d `divides` n = n `mod` d == 0

infixl 8 #
(#) :: a -> (a -> b) -> b
a # f = f a
