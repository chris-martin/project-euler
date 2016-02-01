{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Euler.Problem.Problem11 (answer) where

import Data.FileEmbed     ( embedFile )
import Data.Text          ( Text )
import Data.Text.Encoding ( decodeUtf8 )

import qualified Data.Either   as Either
import qualified Data.Text      as Text
import qualified Data.Text.Read as Read

import Euler.Util.List    ( sliding, transpose )

answer :: Integer
answer = maximum $ map product groups
  where

    groups = foldMap (sliding 4) (concat grids)

    grids = [ grid
            , transpose grid
            , shift grid
            , (shift . reverseRows) grid ]

    shift rows = transpose $ map shiftRow $ zip rows [0..]
      where shiftRow (row, i) = (replicate i 0) ++ row ++
                                (replicate (2 * (length row)) 0)

    reverseRows rows = map reverse rows

grid :: [[Integer]]
grid = map parseLine $ Text.lines inputText
  where parseLine = map fst . Either.rights . (map Read.decimal) . Text.words

inputText :: Text
inputText = decodeUtf8 $(embedFile "../problems/11-data.txt")
