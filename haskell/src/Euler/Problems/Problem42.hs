module Euler.Problems.Problem42 (answer) where

import Euler.Util.WordScore
import Data.Text (Text)
import qualified Data.Text as Text

answer :: Text -> Integer
answer = fromIntegral . length . filter isTriangleWord . parseWords
  where isTriangleWord = isTriangleNum . wordScore

triangles :: Integral a => [a]
triangles = map (\n -> (n * (n + 1)) `div` 2) [1..]

isTriangleNum :: Integral a => a -> Bool
isTriangleNum v = elem v (takeWhile (<= v) triangles)

parseWords :: Text -> [String]
parseWords = map Text.unpack
           . Text.splitOn (Text.pack ",")
           . Text.filter (/= '"')
