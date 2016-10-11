-- | The system of mapping letters to integers used by Euler
-- problems 22 and 42.

module Euler.Util.WordScore
    ( letterScore
    , wordScore
    , wordScoreText
    ) where

-----------------------------------------------------------------

import qualified Data.Char as Char
import qualified Data.Text as Text
import           Data.Text (Text)

wordScore :: String -> Integer
-- ^ The sum of the letter scores in a string.
--
-- > C   o    l    i   n
-- > 3 + 15 + 12 + 9 + 14
--
-- >>> wordScore "Colin"
-- 53

wordScoreText :: Text -> Integer
-- ^ Same as 'wordScore', but for 'Text' instead of 'String'.

letterScore :: Char -> Int
-- ^ Maps the letters @['A'..'Z']@ to @[1..26]@, case-insensitively.
--
-- >>> map letterScore "AaBZ"
-- [1,1,2,26]

-----------------------------------------------------------------

wordScore = sum . map (fromIntegral . letterScore)

wordScoreText = wordScore . Text.unpack

letterScore c = (Char.ord (Char.toUpper c)) - (Char.ord 'A') + 1
