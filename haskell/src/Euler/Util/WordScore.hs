-- | The system of mapping letters to integers used by Euler
-- problems 22 and 42.

module Euler.Util.WordScore
    ( letterScore
    , wordScore
    ) where

-----------------------------------------------------------------

import qualified Data.Char as Char

wordScore :: String -> Integer
-- ^ The sum of the letter scores in a string.
--
-- > C   o    l    i   n
-- > 3 + 15 + 12 + 9 + 14
--
-- >>> wordScore "Colin"
-- 53

letterScore :: Char -> Int
-- ^ Maps the letters @['A'..'Z']@ to @[1..26]@, case-insensitively.
--
-- >>> letterScore 'A'
-- 1
-- >>> letterScore 'a'
-- 1
-- >>> letterScore 'B'
-- 2
-- >>> letterScore 'Z'
-- 26

-----------------------------------------------------------------

wordScore = sum . map (fromIntegral . letterScore)

letterScore c = (Char.ord (Char.toUpper c)) - (Char.ord 'A') + 1
