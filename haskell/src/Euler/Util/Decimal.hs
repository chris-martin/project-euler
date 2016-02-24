module Euler.Util.Decimal
    ( repetendLength
    ) where

import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci

-- | The length of the repeating portion of a rational number's decimal
-- representation. If a decimal doesn't repeat, its repetend length is 0.
repetendLength :: Rational -> Int
repetendLength r =
    case Sci.fromRationalRepetend Nothing r of
      Right (s, Nothing) -> 0
      Right (s, Just i)  -> -(Sci.base10Exponent s) - i
