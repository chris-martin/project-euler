{-# LANGUAGE OverloadedStrings #-}

module Euler.Problems.Problem42
    ( answer, parseWords, isTriangleWord
    ) where

import Euler.Prelude

import Euler.Util.FigurateNumbers (isTriangle)
import Euler.Util.WordScore       (wordScoreText)

import qualified Data.Text as Text

--------------------------------------------------------------------

type InputText = Text
-- ^ The contents of the input file: a comma-delimited
--   list of quoted strings.

answer :: InputText -> Integer
-- ^ The number of triangle words in the input.

parseWords :: InputText -> [Text]

isTriangleWord :: Text -> Bool
-- ^ Whether a word's score is a triangle number.

--------------------------------------------------------------------

answer = fromIntegral . length . filter isTriangleWord . parseWords

parseWords = Text.splitOn "," . Text.filter (/= '"')

isTriangleWord = isTriangle . wordScoreText
