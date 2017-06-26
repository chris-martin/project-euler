{-# LANGUAGE OverloadedStrings #-}

module Euler.Problems.Problem42
  ( answer
  , parseWords
  , isTriangleWord
  ) where

import Euler.Prelude

import Euler.Util.FigurateNumbers (isTriangle)
import Euler.Util.WordScore (wordScoreText)

import qualified Data.List as List
import qualified Data.Text as Text

{- |

The contents of the input file: a comma-delimited list of quoted strings.

-}
type InputText = Text

{- |

The number of triangle words in the input.

-}
answer :: InputText -> Integer
answer =
  parseWords >>>
  List.filter isTriangleWord >>>
  List.length >>>
  fromIntegral

parseWords :: InputText -> [Text]
parseWords =
  Text.filter (/= '"') >>>
  Text.splitOn ","

{- |

Whether a word's score is a triangle number.

-}
isTriangleWord :: Text -> Bool
isTriangleWord =
  wordScoreText >>>
  isTriangle
