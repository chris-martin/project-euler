module Euler.Problems.Problem11
    ( Grid, GridText, answer, parseGrid ) where

import Euler.Prelude

import Euler.Util.List (sliding, transpose)

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead

type GridText = Text
-- ^ The contents of the input file: a grid of base-10
--   integers delimited by spaces and newlines.

answer :: GridText -> Integer
-- ^ The greatest product of four adjacent numbers in the same direction.

answer = maximum . map product . groups . parseGrid

type Grid = [[Integer]]

groups :: Grid -> Grid
groups g = foldMap (sliding 4) (concat $ gridPermutations g)

gridPermutations :: Grid -> [Grid]
gridPermutations g = ($ g) <$> fs
  where
    fs = [ id
         , transpose
         , shift
         , shift . map reverse
         ]

shift :: Grid -> Grid
shift rows = (transpose . map shiftRow) (zip rows [0..])
  where
    shiftRow (row, i) = concat [ replicate i 0
                               , row
                               , replicate (2 * length row) 0
                               ]

parseGrid :: GridText -> Grid
parseGrid = map parseLine . Text.lines
  where
    parseLine = map fst
              . Either.rights
              . map TextRead.decimal
              . Text.words
