{-# LANGUAGE OverloadedStrings #-}

module Euler.Problems.Problem22
    ( -- $setup
      InputText
    , parseNames
    , unquote
    , answer
    ) where

import Euler.Prelude

import Euler.Util.WordScore

import qualified Data.Text as Text

-- $setup
-- >>> :set -XOverloadedStrings

--------------------------------------------------------------------------------

-- | The contents of the input file: a comma-delimited list of quoted strings.
type InputText = Text

type Name = Text

-- | The answer to the problem, given the input text.
--
-- >>> answer "\"Bob\",\"Alice\""
-- 68
--
-- >       A   l    i   c   e           B   o    b
-- > (1 * (1 + 12 + 9 + 3 + 5)) + (2 * (2 + 15 + 2))
answer :: InputText -> Integer
answer text =
    sum scores
  where
    names = sort (parseNames text)
    scores = zipWith (*) [1..] (map (wordScore . Text.unpack) names)

-- | Parse the input text for this problem.
--
-- >>> parseNames "\"Alice\",\"Bob\""
-- ["Alice","Bob"]
parseNames :: InputText -> [Name]
parseNames =
  map (unquote . Text.strip) . Text.splitOn ","

-- |
-- >>> unquote "\"Alice\""
-- "Alice"
unquote :: Text -> Text
unquote =
    Text.dropWhile isQ . Text.dropWhileEnd isQ
  where
    isQ = (== '"')
