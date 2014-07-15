module Problem6 where

answer :: Integer
answer = (square $ sum xs) - (sum $ map square xs)
  where
    square n = n ^ (2 :: Integer)
    xs = [1..100]
