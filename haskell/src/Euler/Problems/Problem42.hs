{-# LANGUAGE OverloadedStrings #-}

module Euler.Problems.Problem42
    ( answer, parseWords, isTriangleWord ) where

import           Data.Text (Text)
import qualified Data.Text as Text

import Euler.Util.FigurateNumbers (isTriangle)
import Euler.Util.WordScore       (wordScore, wordScoreText)

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
