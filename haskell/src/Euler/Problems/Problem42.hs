{-# LANGUAGE OverloadedStrings #-}

module Euler.Problems.Problem42 (answer) where

import Data.Text (Text)

import qualified Data.Char as Char
import qualified Data.Text as Text

import Prelude ( (.), (+), (-), (*), (/=), (<=)
               , div, elem, filter, length, map, sum, takeWhile
               , Char, Int )

answer :: Text -> Int
answer t = length (filter isTriangleWord words)
  where
    triangles = map (\n -> (n * (n + 1)) `div` 2) [1..]
    isTriangleNum v = elem v (takeWhile (<= v) triangles)
    isTriangleWord = isTriangleNum . wordValue

    words = Text.splitOn "," (Text.filter (/= '"') t)

-- | wordValue "Sky" = 19 + 11 + 25 = 55
wordValue :: Text -> Int
wordValue = sum . (map letterValue) . Text.unpack . Text.toUpper

-- | A = 1, B = 2, etc.
letterValue :: Char -> Int
letterValue c = (Char.ord c) - (Char.ord 'A') + 1
