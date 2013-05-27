module Problem2 where

answer :: Integer
answer = fibs
  # takeWhile(< 4000000)
  # filter(\x -> x `mod` 2 == 0)
  # sum

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

infixl 8 #
(#) :: a -> (a -> b) -> b
a # f = f a
