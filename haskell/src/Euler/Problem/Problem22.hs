{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Euler.Problem.Problem22 (answer) where

import Data.Char          ( ord )
import Data.FileEmbed     ( embedFile )
import Data.List          ( sort )
import Data.Text          ( Text )
import Data.Text.Encoding ( decodeUtf8 )

import qualified Data.Text as Text

answer :: Integer
answer = sum scores where
    names = sort $ parseNames inputText
    scores = zipWith (*) [1..] $ map wordScore names

-- "COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53"
wordScore :: Text -> Integer
wordScore = sum . (map $ fromIntegral . letterScore) . Text.unpack

-- A = 1, B = 2, etc.
letterScore :: Char -> Int
letterScore c = (ord c) - (ord 'A') + 1

parseNames :: Text -> [Text]
parseNames = (map parseName) . (Text.splitOn ",") . Text.toUpper where
    parseName = unquote . Text.strip
    unquote = (Text.dropWhile isQ) . (Text.dropWhileEnd isQ)
    isQ = (== '"')

inputText :: Text
inputText = decodeUtf8 $(embedFile "../problems/22-data.txt")
