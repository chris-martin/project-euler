module Euler.Util.Prime
  ( smallestPrimeFactor
  , largestPrimeFactor
  ) where

import Euler.Prelude

primes :: ZList BigInt
primes = ZList.filter BigInt.prime $ BigInt.iterate (\x -> x + 1)

-- | @'smallestPrimeFactor' n@ is the smallest prime factor @p@
-- | such that @p@ divides @n@.
smallestPrimeFactor :: BigInt -> BigInt
smallestPrimeFactor n = head $ filter (\x -> x `divides` n) primes
  where
  divides a b = b `mod` a == 0

-- | @'largestPrimeFactor' n@ is the largest prime factor @p@
-- | such that @p@ divides @n@.
-- |
-- | >>> largestPrimeFactor 2
-- | 2
-- |
-- | >>> largestPrimeFactor 3
-- | 3
-- |
-- | >>> largestPrimeFactor 4
-- | 2
-- |
-- | >>> largestPrimeFactor 99
-- | 11
-- |
-- | prop> forAll (elements $ take 50 primes) (\p -> largestPrimeFactor p == p)
largestPrimeFactor :: BigInt -> BigInt
largestPrimeFactor n
    | isPrime n = fromIntegral n
    | otherwise = largestPrimeFactor $ n `div` smallestPrimeFactor n
