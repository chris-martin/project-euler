-- | The system of mapping letters to integers used by Euler
-- problems 22 and 42.

module Euler.Util.WordScore
  ( letterScore
  , wordScore
  , wordScoreText
  ) where

import Euler.Prelude

import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.Text as Text

-----------------------------------------------------------------

{- |

The sum of the letter scores in a string.

> C   o    l    i   n
> 3 + 15 + 12 + 9 + 14

>>> wordScore "Colin"
53

-}
wordScore :: String -> Integer
wordScore =
  fmap (fromIntegral . letterScore) >>> Foldable.sum

{- |

Same as 'wordScore', but for 'Text' instead of 'String'.

-}
wordScoreText :: Text -> Integer
wordScoreText =
  Text.unpack >>> wordScore

{- |

Maps the letters @['A'..'Z']@ to @[1..26]@, case-insensitively.

>>> map letterScore "AaBZ"
[1,1,2,26]

-}
letterScore :: Char -> Int
letterScore c =
  Char.ord (Char.toUpper c) - Char.ord 'A' + 1
