module Euler.Problems.Problem11
  ( Grid
  , GridText
  , answer
  , parseGrid
  ) where

import Euler.Prelude

import Euler.Util.List (sliding, transpose)

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead

-- | The contents of the input file: a grid of base-10
-- integers delimited by spaces and newlines.
type GridText = Text

-- | The greatest product of four adjacent numbers in the same direction.
answer :: GridText -> Integer

answer = maximum . map product . groups . parseGrid

type Grid = [[Integer]]

groups :: Grid -> Grid
groups g = foldMap (sliding 4) (concat $ gridPermutations g)

gridPermutations :: Grid -> [Grid]
gridPermutations g =
  ($ g) <$>
  [ id
  , transpose
  , shift
  , shift . map reverse
  ]

shift :: Grid -> Grid
shift rows =
  zip rows [0..] &
  fmap (\(row, i) -> concat [ replicate i 0
                            , row
                            , replicate (2 * length row) 0
                            ]) &
  transpose

parseGrid :: GridText -> Grid
parseGrid text =
  text & Text.lines <&> \line ->
  line & Text.words & fmap TextRead.decimal & Either.rights & fmap fst
