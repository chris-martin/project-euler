module Euler.Util.Decimal
    ( repetendLength
    ) where

import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci

repetendLength :: Rational -> Int
repetendLength r =
    case Sci.fromRationalRepetend Nothing r of
      Right (s, Nothing) -> 0
      Right (s, Just i)  -> -(Sci.base10Exponent s) - i
