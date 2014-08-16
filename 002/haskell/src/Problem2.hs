module Problem2 where

answer :: Integer
answer = sum $ filter even $ takeWhile (< bound) fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
    bound = 4 * million
    million = 10 ^ (6 :: Integer)
