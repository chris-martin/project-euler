module Euler.Util.Prime
  (
    -- $setup

    factorizations
  , factorizationsAsc
  , primes
  , isPrime

  -- * Finding prime factors
  , primeFactors
  , smallestPrimeFactor
  , largestPrimeFactor

  -- * Divisors
  , divisors
  , countDivisors
  , divisorsOfPrimeProduct

  -- * Proper divisors
  -- $properDivisors
  , properDivisors
  , properDivisorsOfPrimeProduct
  ) where

import Euler.Prelude

import Euler.Util.Arithmetic (divides)
import Euler.Util.List (untilNothing)

import qualified Euler.Util.FrontierSearch as FS
import qualified Euler.Util.Inf as Inf

import Data.List (length, product, takeWhile)

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map'
import qualified Data.MultiSet as MultiSet
import qualified Math.Combinatorics.Multiset as MS

{- $setup

>>> import Test.QuickCheck
>>> import Data.List.Ordered (isSorted)

-}

--------------------------------------------------------------------------------

{- |

@'factorizations' n@ gives the prime factorizations of numbers in @[1..n]@. Each
factorization is sorted.

prop> factorizations 50 == Map'.fromList [(n, primeFactors n) | n <- [1..50]]

-}
factorizations :: Integral a => a -> Map a [a]
factorizations n = toMap $ [] : f []
  where
    f tail = (fold . untilNothing . fmap g) primes
      where
        g p = let fs = p : tail
              in  if product fs > n then Nothing else Just $ fs : f fs

    toMap = Map'.fromList . fmap (liftA2 (,) Foldable.product sort)

{- |

>>> take 9 factorizationsAsc
[[],[2],[3],[2,2],[5],[2,3],[7],[2,2,2],[3,3]]

prop> take 50 factorizationsAsc == (primeFactors <$> [1..50])

-}
factorizationsAsc :: Integral a => [[a]]
factorizationsAsc = (\(_, fs, _) -> MultiSet.toAscList fs) <$> FS.searchNodes FS.Conf
    { FS.start = [(1, MultiSet.empty, Inf.fromList primes)]
    , FS.next = \(n, factors, higherPrimes) ->
       let h:hs = Inf.toList higherPrimes in
           (if MultiSet.size factors <= 1
               then [(h, MultiSet.singleton h, Inf.fromList hs)]
               else []) <>
           ((\f -> (n*f, MultiSet.insert f factors, higherPrimes)) <$> takeWhile (<h) primes)
    , FS.nodeValue = \(n, _, _) -> n
    }


--------------------------------------------------------------------------------
--  Finding prime factors
--------------------------------------------------------------------------------

{- |

prop> forAll (listOf $ elements $ take 20 primes) (\fs -> primeFactors (product fs) == sort fs)

prop> \(Positive n) -> isSorted (primeFactors n)

-}
primeFactors :: (Integral a, Integral b) => a -> [b]
primeFactors n =
    sort $ primeFactors' [] n
  where
    primeFactors' :: (Integral x, Integral y) => [x] -> y -> [x]
    primeFactors' f' 1 = f'
    primeFactors' f' n' = primeFactors' f'' n''
      where
        f'' = d : f'
        n'' = fromIntegral n' `div` fromIntegral d
        d = smallestPrimeFactor n'

{- |

@'smallestPrimeFactor' n@ is the smallest prime factor @p@ such that @p@ divides
@n@.

-}
smallestPrimeFactor :: (Integral a, Integral b) => a -> b
smallestPrimeFactor n =
  List.head $ List.filter (`divides` fromIntegral n) primes

{- |

@'largestPrimeFactor' n@ is the largest prime factor @p@ such that @p@ divides
@n@.

>>> largestPrimeFactor 2
2

>>> largestPrimeFactor 3
3

>>> largestPrimeFactor 4
2

>>> largestPrimeFactor 99
11

prop> forAll (elements $ take 50 primes) (\p -> largestPrimeFactor p == p)

-}
largestPrimeFactor :: (Integral a, Integral b) => a -> b
largestPrimeFactor n
    | isPrime n = fromIntegral n
    | otherwise = largestPrimeFactor $ n `div` smallestPrimeFactor n


--------------------------------------------------------------------------------
--  Divisors
--------------------------------------------------------------------------------

{- |

@'countDivisors' n@ is the number of integers d from @[1..n]@ such that @d@
divides @n@.

-}
countDivisors :: (Integral a, Integral b) => a -> b
countDivisors =
  Foldable.product . fmap (fromIntegral . (+1) . List.length) . group . primeFactors

{- |

The divisors of a number, in no particular order.

-}
divisors :: Integral a => a -> [a]

{- |

The divisors of @'product' fs@, where @fs@ are all prime, in no particular
order.

-}
divisorsOfPrimeProduct :: Integral a => [a] -> [a]


--------------------------------------------------------------------------------
--  Proper divisors
--------------------------------------------------------------------------------

{- $properDivisors

"Proper divisors" are divisors of a number, other than the number itself.

-}

{- |

The proper divisors of a number, in no particular order.

Example from Euler problem 21:

>>> sort (properDivisors 220)
[1,2,4,5,10,11,20,22,44,55,110]

-}
properDivisors :: Integral a => a -> [a]

{- |

The proper divisors of @'product' fs@, where @fs@ are all prime, in no
particular order.

-}
properDivisorsOfPrimeProduct :: Integral a => [a] -> [a]


--------------------------------------------------------------------------------

divisorsOfPrimeProduct       xs = fmap product $ maxKSubMultisets (length xs)     xs
properDivisorsOfPrimeProduct xs = fmap product $ maxKSubMultisets (length xs - 1) xs

divisors       = divisorsOfPrimeProduct       . primeFactors
properDivisors = properDivisorsOfPrimeProduct . primeFactors

maxKSubMultisets :: Ord a => MS.Count -> [a] -> [[a]]
maxKSubMultisets maxK xs =
    let set = MS.fromList xs
    in do k      <- [0 .. maxK]
          subset <- MS.kSubsets k set
          return $ MS.toList subset
