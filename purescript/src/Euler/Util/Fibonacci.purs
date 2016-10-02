module Euler.Util.Fibonacci (fibs) where

import Euler.Prelude

-- | >>> take 13 fibs
-- | [0,1,1,2,3,5,8,13,21,34,55,89,144]
fibs :: ZList.List Int
fibs = fst <$> ZList.iterate (\(Tuple a b) -> Tuple b (a + b)) (Tuple 0 1)

bigFibs :: ZList.List BigInt
bigFibs = fst <$> ZList.iterate (\(Tuple a b) -> Tuple b (a + b)) (Tuple (BigInt.fromInt 0) (BigInt.fromInt 1))
