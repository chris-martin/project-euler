{- |

In England the currency is made up of pound, £, and pence, p.
There are eight coins in general circulation:
1p, 2p, 5p, 10p, 20p, 50p, £1 (100p), and £2 (200p).

-}
module Euler.Problems.Problem31 (answer, ways) where

import Euler.Prelude

import qualified Data.Foldable as Foldable
import qualified Data.List as List

{- |

The number of ways to make £2 using any number of coins.

-}
answer :: Natural
answer = ways 200 [200, 100, 50, 20, 10, 5, 2, 1]

ways
  :: Natural   -- ^ Target amount
  -> [Natural] -- ^ Denominations (in descending order for best performance)
  -> Natural   -- ^ Number of ways to make the target amount using the
               --   denominations

ways 0 _  = 1  -- There's exactly one way to make no money
ways _ [] = 0  -- You can't anything with no denominations

ways target (x:xs) =
  Foldable.sum $
  -- r: How much to make with the first denomination
  List.takeWhile (<= target) $ List.iterate (+ x) 0 <&> \r ->
  -- How many ways to make the rest without that denomination
  ways (target - r) xs
