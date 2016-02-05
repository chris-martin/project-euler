module Euler.Problem.Problem23 (answer) where

import qualified Data.Map      as Map
import qualified Data.Sequence as Seq
import qualified Data.Set      as Set

import Euler.Util.Prime ( properDivisorsOfPrimeProduct, factorizations )

answer :: Integer
answer = sum $ (Set.fromList [1..max]) Set.\\ (Set.fromList xs) where

    max = 28123

    divisorSums = fmap (sum . properDivisorsOfPrimeProduct) (factorizations max)
    d = (divisorSums Map.!)

    abundants = Seq.fromList $ filter (\n -> d n > n) [2..max]
    ab = (abundants `Seq.index`)
    l = (length abundants) - 1

    xs = do x <- [0..l]
            y <- [x..l]
            return $ (ab x) + (ab y)
