module Euler.Problems.Problem42 (answer) where

import Euler.Util.FigurateNumbers (isTriangle)
import Euler.Util.WordScore (wordScore)
import Data.Text (Text)
import qualified Data.Text as Text

answer :: Text -> Integer
answer = fromIntegral . length . filter isTriangleWord . parseWords
  where isTriangleWord = isTriangle . wordScore

parseWords :: Text -> [String]
parseWords = map Text.unpack
           . Text.splitOn (Text.pack ",")
           . Text.filter (/= '"')
