module Euler.Problems.Problem11
  ( Grid
  , GridText
  , answer
  , parseGrid
  ) where

import Euler.Prelude

import Euler.Util.List (sliding, transpose)

import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead

-- | The contents of the input file: a grid of base-10
-- integers delimited by spaces and newlines.
type GridText = Text

-- | The greatest product of four adjacent numbers in the same direction.
answer :: GridText -> Integer

answer = List.maximum . fmap List.product . groups . parseGrid

type Grid = [[Integer]]

groups :: Grid -> Grid
groups g =
  foldMap (sliding 4) (fold $ gridPermutations g)

gridPermutations :: Grid -> [Grid]
gridPermutations g =
  ($ g) <$>
  [ id
  , transpose
  , shift
  , shift . fmap List.reverse
  ]

shift :: Grid -> Grid
shift =
  List.zip [0..] >>>
  fmap (\(i, row) -> fold [ List.replicate i 0
                          , row
                          , List.replicate (2 * List.length row) 0
                          ]) >>>
  transpose

parseGrid :: GridText -> Grid
parseGrid text =
  text & Text.lines <&> \line ->
  line & Text.words & fmap TextRead.decimal & Either.rights & fmap fst
