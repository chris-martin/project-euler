module Problem1 where

answer :: Integer
answer = sum multiples
  where
    multiples = filter isMultiple [1..999]
    isMultiple n = any (`divides` n) [3, 5]
    d `divides` n = n `mod` d == 0
