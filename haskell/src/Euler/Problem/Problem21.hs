module Euler.Problem.Problem21 (answer) where

import qualified Data.Map.Strict as Map

import Euler.Util.Prime ( divisorsOfPrimeProduct, factorizations )

answer :: Integer
answer = sum $ filter isAmicable [2 .. max] where
    max = 9999
    divisorSums = Map.fromList $
                  map (fmap $ sum . divisorsOfPrimeProduct) $
                  factorizations max
    isAmicable i = maybe False f $ Map.lookup i divisorSums
                   where f x = x /= i && Map.lookup x divisorSums == Just i

-- // function : n in [2, max] -> Some(sum of proper divisors of n)
-- lazy val divisorSums = {
-- val ds = new Array[Int](max + 1)
-- for (fs <- factorizations())
--   ds.update(fs.product, properDivisors(fs).sum)
-- i: Int => if (i >= 2 && i <= max) Some(ds(i)) else None
-- }
--
-- def isAmicable(i: Int): Boolean =
-- divisorSums(i).exists(x => x != i && divisorSums(x) == Some(i))
