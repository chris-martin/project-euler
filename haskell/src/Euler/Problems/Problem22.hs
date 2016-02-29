module Euler.Problems.Problem22
    ( answer
    , parseNames
    ) where

import Euler.Util.WordScore

import Data.List (sort)
import Data.Text (Text)

import qualified Data.Char as Char
import qualified Data.Text as Text

-------------------------------------------------------

answer :: Text -> Integer
-- ^ The answer to the problem, given the input text.
--
-- >>> answer (Text.pack "\"Bob\",\"Alice\"")
-- 68
--
-- >       A   l    i   c   e           B   o    b
-- > (1 * (1 + 12 + 9 + 3 + 5)) + (2 * (2 + 15 + 2))

parseNames :: Text -> [String]
-- ^ Parse the input text for this problem.
--
-- >>> parseNames (Text.pack "\"Alice\",\"Bob\"")
-- ["Alice","Bob"]

-------------------------------------------------------

answer text = sum scores
  where
    names = sort (parseNames text)
    scores = zipWith (*) [1..] (map wordScore names)

parseNames = map parseName . splitOnComma
  where
    splitOnComma = Text.splitOn (Text.pack ",")
    parseName = Text.unpack . unquote . Text.strip
    unquote = Text.dropWhile isQ . Text.dropWhileEnd isQ
    isQ = (== '"')
