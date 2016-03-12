module Euler.Problems.Problem11 (answer) where

import Euler.Util.List (sliding, transpose)

import Data.Text (Text)

import qualified Data.Either    as Either
import qualified Data.Text      as Text
import qualified Data.Text.Read as TextRead

answer :: Text -> Integer
answer = maximum . (map product) . groups . parseGrid

type Grid = [[Integer]]

groups :: Grid -> Grid
groups g = foldMap (sliding 4) (concat (gridPermutations g))

gridPermutations :: Grid -> [Grid]
gridPermutations g = [ g, transpose g, shift g, (shift . (map reverse)) g ]

shift :: Grid -> Grid
shift rows = (transpose . (map shiftRow)) (zip rows [0..])
  where shiftRow (row, i) = concat [ replicate i 0
                                   , row
                                   , replicate (2 * (length row)) 0
                                   ]

parseGrid :: Text -> Grid
parseGrid = (map parseLine) . Text.lines
  where parseLine = map fst . Either.rights . (map TextRead.decimal) . Text.words
