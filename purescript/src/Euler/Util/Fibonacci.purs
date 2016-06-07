module Euler.Util.Fibonacci (fibs) where

import Prelude ((+), (<$>), Unit, unit)

import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.List.Lazy (List, iterate)
import Data.Tuple (Tuple(..), fst, snd)

-- | >>> take 13 fibs
-- | [0,1,1,2,3,5,8,13,21,34,55,89,144]
fibs :: List BigInt
fibs = fst <$> iterate (\(Tuple a b) -> Tuple b (a + b)) (Tuple (BigInt.fromInt 0) (BigInt.fromInt 1))
