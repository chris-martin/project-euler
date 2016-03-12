module Euler.Problems.Problem23 (answer) where

import Data.Set         ((\\))
import Euler.Util.Prime (factorizations, properDivisorsOfPrimeProduct)

import qualified Data.Map      as Map
import qualified Data.Sequence as Seq
import qualified Data.Set      as Set

answer :: Integer
answer = answerBounded 28123

answerBounded :: Integer -> Integer
answerBounded bound = sum $
    Set.fromList [1..bound] \\
    Set.fromList (xs bound)

xs :: Integral a => a -> [a]
xs bound = do x <- [0..l]
              y <- [x..l]
              return ((ab x) + (ab y))
  where
    divisorSums = fmap (sum . properDivisorsOfPrimeProduct) (factorizations bound)
    d = (divisorSums Map.!)

    abundants = (Seq.fromList . (filter (\n -> d n > n))) [2..bound]
    ab = (abundants `Seq.index`)
    l = (length abundants) - 1
