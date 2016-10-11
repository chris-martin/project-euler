{-# LANGUAGE OverloadedStrings #-}

module Euler.Problems.Problem22
    ( -- $setup
      InputText
    , parseNames
    , unquote
    , answer
    ) where

import Euler.Util.WordScore

import Data.List (sort)
import Data.Text (Text)

import qualified Data.Char as Char
import qualified Data.Text as Text

-- $setup
-- >>> :set -XOverloadedStrings

-------------------------------------------------------

type InputText = Text
-- ^ The contents of the input file: a comma-delimited
--   list of quoted strings.

type Name = Text

answer :: InputText -> Integer
-- ^ The answer to the problem, given the input text.
--
-- >>> answer "\"Bob\",\"Alice\""
-- 68
--
-- >       A   l    i   c   e           B   o    b
-- > (1 * (1 + 12 + 9 + 3 + 5)) + (2 * (2 + 15 + 2))

parseNames :: InputText -> [Name]
-- ^ Parse the input text for this problem.
--
-- >>> parseNames "\"Alice\",\"Bob\""
-- ["Alice","Bob"]

unquote :: Text -> Text
-- ^
-- >>> unquote "\"Alice\""
-- "Alice"

-------------------------------------------------------

answer text = sum scores
  where
    names = sort (parseNames text)
    scores = zipWith (*) [1..] (map (wordScore . Text.unpack) names)

parseNames = map (unquote . Text.strip) . Text.splitOn ","

unquote = Text.dropWhile isQ . Text.dropWhileEnd isQ
  where
    isQ = (== '"')
