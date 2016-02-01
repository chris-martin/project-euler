module Euler.Util.Prime
    ( primeFactors
    , lowestPrimeFactor
    , countDivisors
    ) where

import Data.List (group, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Numbers.Primes (primes)

primeFactors :: (Integral a, Integral b) => a -> [b]
primeFactors n = sort $ primeFactors' [] n where
    primeFactors' :: (Integral x, Integral y) => [x] -> y -> [x]
    primeFactors' f' 1 = f'
    primeFactors' f' n' = primeFactors' f'' n'' where
        f'' = d : f'
        n'' = (fromIntegral n') `div` (fromIntegral d)
        d = lowestPrimeFactor n'

-- | @'lowestPrimeFactor' n@ is the smallest prime factor @p@
-- such that @p@ divides @n@.
lowestPrimeFactor :: (Integral a, Integral b) => a -> b
lowestPrimeFactor n = head $ filter (`divides` (fromIntegral n)) primes
    where n' `divides` d = d `mod` n' == 0

-- | @'countDivisors' n@ is the number of integers d from @[1..n]@
-- such that @d@ divides @n@.
countDivisors :: (Integral a, Integral b) => a -> b
countDivisors = product . map (fromIntegral . (+1) . length) . group . primeFactors
