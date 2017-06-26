module Euler.Problems.Problem46
  ( answer
  , goldbachNumbers
  , squareDoubles
  ) where

import Euler.Prelude

import Euler.Util.List (adjustEach)
import Euler.Util.Prime (isPrime, primes)

import qualified Euler.Util.FrontierSearch as FS
import qualified Euler.Util.Inf as Inf

import qualified Data.List as List
import qualified Data.List.Ordered as OL

{- |

The smallest odd composite that cannot be written as the sum of a prime and
twice a square.

-}
answer :: Integer
answer =
  List.head . List.filter (not . isPrime) $ OL.minus [3, 5..] goldbachNumbers

{- |

All numbers that can be written as the sum of a prime and twice a square, in
ascending order.

We're calling these number "goldbach numbers" after Christian Goldbach; this is
just made-up terminology here for lack of a better word.

>>> take 20 goldbachNumbers
[4,5,7,9,10,11,13,15,19,20,21,23,25,27,29,31,33,34,35,37]

-}
goldbachNumbers :: Integral a => [a]
goldbachNumbers =
  FS.searchValues FS.Conf
  { FS.start     = [[Inf.fromList primes, Inf.fromList squareDoubles]]
  , FS.next      = adjustEach $ Inf.fromList . List.tail . Inf.toList
  , FS.nodeValue = List.sum . (List.head . Inf.toList <$>)
  }

{- |

prop> take 100 squareDoubles == map (\n -> 2 * n^2) [1..100]

-}
squareDoubles :: Integral a => [a]
squareDoubles =
  List.scanl1 (+) [2, 6 ..]
