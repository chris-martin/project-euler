-- |
-- In England the currency is made up of pound, £, and pence, p.
-- There are eight coins in general circulation:
-- 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

module Euler.Problems.Problem31 (answer, ways) where

answer :: (Integral a) => a
-- ^ The number of ways to make £2 using any number of coins.

answer = ways 200 [200, 100, 50, 20, 10, 5, 2, 1]

ways :: (Integral a)
     => a   -- ^ Target amount
     -> [a] -- ^ Denominations (in descending order for
            --   best performance)
     -> a   -- ^ Number of ways to make the target amount
            --   using the denominations

ways 0 _  = 1  -- There's exactly one way to make no money
ways _ [] = 0  -- You can't anything with no denominations

ways target (x:xs) = sum $ do
    -- r: How much to make with the first denomination
    r <- takeWhile (<= target) $ iterate (+ x) 0
    -- How many ways to make the rest without that denomination
    return $ ways (target - r) xs
