module Euler.Util.Prime.Factoring
    ( primeFactors
    , smallestPrimeFactor
    , largestPrimeFactor
    , countDivisors
    , divisorsOfPrimeProduct
    , properDivisorsOfPrimeProduct
    ) where

import qualified Math.Combinatorics.Multiset as MS

import Data.List           ( group, sort )
import Data.Map            ( Map )
import Data.Maybe          ( fromMaybe )
import Data.Numbers.Primes ( isPrime, primes )

import Euler.Util.Arithmetic       ( divides )

primeFactors :: (Integral a, Integral b) => a -> [b]
primeFactors n = sort $ primeFactors' [] n where
    primeFactors' :: (Integral x, Integral y) => [x] -> y -> [x]
    primeFactors' f' 1 = f'
    primeFactors' f' n' = primeFactors' f'' n'' where
        f'' = d : f'
        n'' = (fromIntegral n') `div` (fromIntegral d)
        d = smallestPrimeFactor n'

-- | @'smallestPrimeFactor' n@ is the smallest prime factor @p@
-- such that @p@ divides @n@.
smallestPrimeFactor :: (Integral a, Integral b) => a -> b
smallestPrimeFactor n = head $ filter (`divides` (fromIntegral n)) primes

-- | @'largestPrimeFactor' n@ is the largest prime factor @p@
-- such that @p@ divides @n@.
largestPrimeFactor :: (Integral a, Integral b) => a -> b
largestPrimeFactor n
    | isPrime n = fromIntegral n
    | otherwise = largestPrimeFactor $ n `div` smallestPrimeFactor n

-- | @'countDivisors' n@ is the number of integers d from @[1..n]@
-- such that @d@ divides @n@.
countDivisors :: (Integral a, Integral b) => a -> b
countDivisors = product . map (fromIntegral . (+1) . length) . group . primeFactors

-- | The divisors of @'product' fs@, where @fs@ are all prime,
-- in no particular order.
divisorsOfPrimeProduct :: Integral a => [a] -> [a]
divisorsOfPrimeProduct xs = map product $ maxKSubMultisets (length xs) xs

-- | The proper divisors of @'product' fs@, where @fs@ are all prime,
-- in no particular order. "Proper divisors" means divisors other than
-- the number itself.
properDivisorsOfPrimeProduct :: Integral a => [a] -> [a]
properDivisorsOfPrimeProduct xs = map product $ maxKSubMultisets (length xs - 1) xs

maxKSubMultisets :: Ord a => MS.Count -> [a] -> [[a]]
maxKSubMultisets maxK xs =
    do k      <- [0 .. maxK]
       subset <- MS.kSubsets k set
       return $ MS.toList subset
    where set = MS.fromList xs
