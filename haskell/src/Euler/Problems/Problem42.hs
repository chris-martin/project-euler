module Euler.Problems.Problem42 (answer) where

import Data.Text (Text)

import qualified Data.Char as Char
import qualified Data.Text as Text

import Prelude ( (.), (+), (-), (*), (/=), (<=)
               , div, elem, filter, length, map, sum, takeWhile
               , Bool, Char, Int )

answer :: Text -> Int
answer = length . (filter isTriangleWord) . parseWords
  where isTriangleWord = isTriangleNum . wordValue

triangles :: [Int]
triangles = map (\n -> (n * (n + 1)) `div` 2) [1..]

isTriangleNum :: Int -> Bool
isTriangleNum v = elem v (takeWhile (<= v) triangles)

parseWords :: Text -> [Text]
parseWords = (Text.splitOn ",") . (Text.filter (/= '"'))

-- | wordValue "Sky" = 19 + 11 + 25 = 55
wordValue :: Text -> Int
wordValue = sum . (map letterValue) . Text.unpack . Text.toUpper

-- | A = 1, B = 2, etc.
letterValue :: Char -> Int
letterValue c = (Char.ord c) - (Char.ord 'A') + 1
