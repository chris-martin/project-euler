{- |

A number /n/ is called abundant if the sum of its proper divisors exceeds /n/.

-}
module Euler.Problems.Problem23
    ( answer
    , answerBounded
    ) where

import Euler.Prelude

import Euler.Util.Prime (factorizations, properDivisorsOfPrimeProduct)

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

-- | The sum of all the positive integers which cannot be written as the sum of
-- two abundant numbers.
answer :: Integer
answer = answerBounded 28123

-- | The sum of all the positive integers /≤ bound/ which cannot be written as
-- the sum of two abundant numbers.
answerBounded :: Integer  -- ^ /bound/
              -> Integer
answerBounded bound =
  sum $
  Set.fromList [1..bound] \\
  Set.fromList (xs bound)

xs :: (Integral a) => a -> [a]
xs bound =
    [ ab x + ab y | x <- [0..l], y <- [x..l] ]
  where
    divisorSums = fmap (sum . properDivisorsOfPrimeProduct)
                       (factorizations bound)
    d = (divisorSums Map.!)

    abundants = (Seq.fromList . filter (\n -> d n > n)) [2..bound]
    ab = (abundants `Seq.index`)
    l = length abundants - 1
