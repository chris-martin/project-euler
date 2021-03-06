module Euler.Util.Decimal
  ( repetendLength
  ) where

import Euler.Prelude

import qualified Data.Scientific as Sci

--------------------------------------------------------------------------------

{- |

The length of the repeating portion of a rational number's decimal
representation. If a decimal doesn't repeat, its repetend length is 0.

>>> [repetendLength (1/n) | n <- [1..10]]
[0,0,1,0,0,1,6,0,1,0]

>>> repetendLength (608/615) -- 0.9(88617)
5

-}
repetendLength :: Rational -> Int
repetendLength r =
  case Sci.fromRationalRepetend Nothing r of
    Right (_, Nothing) -> 0
    Right (s, Just i)  -> negate (Sci.base10Exponent s + i)
    _                  -> undefined
