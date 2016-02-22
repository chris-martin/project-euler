{-# LANGUAGE OverloadedStrings #-}

module Euler.Problems.Problem22 (answer) where

import Data.List (sort)
import Data.Text (Text)

import qualified Data.Char as Char
import qualified Data.Text as Text

answer :: Text -> Integer
answer text = sum scores
  where
    names = sort (parseNames text)
    scores = zipWith (*) [1..] (map wordScore names)

-- | "COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53"
wordScore :: Text -> Integer
wordScore = sum . (map (fromIntegral . letterScore)) . Text.unpack

-- | A = 1, B = 2, etc.
letterScore :: Char -> Int
letterScore c = (Char.ord c) - (Char.ord 'A') + 1

parseNames :: Text -> [Text]
parseNames = (map parseName) . (Text.splitOn ",") . Text.toUpper
  where
    parseName = unquote . Text.strip
    unquote = (Text.dropWhile isQ) . (Text.dropWhileEnd isQ)
    isQ = (== '"')
