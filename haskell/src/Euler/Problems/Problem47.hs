module Euler.Problems.Problem47
  ( answer
  , answerN
  ) where

answer :: Integer
answer =
  answerN (4 :: Int)

-- | The least /n/ consecutive numbers to have /n/ distinct prime factors.
--
-- The first two consecutive numbers to have two distinct prime factors
-- are /14 = 2 × 7/ and /15 = 3 × 5/.
--
-- todo >>> answerN 2
-- 14
--
-- The first three consecutive numbers to have three distinct prime
-- factors are /644 = 2^2 × 7 × 23/, /645 = 3 × 5 × 43/, and
-- /646 = 2 × 17 × 19/.
--
-- todo >>> answerN 3
-- 644
answerN :: (Integral a, Integral b) => a -> b
answerN _ = undefined -- todo
