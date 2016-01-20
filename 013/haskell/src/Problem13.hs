{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Problem13 (answer) where

import Data.Either (rights)
import Data.FileEmbed (embedFile)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Read (decimal)

answer :: String
answer = take 10 $ show $ sum numbers

numbers :: [Integer]
numbers = rights $ map parseLine $ Text.lines inputText
    where parseLine = (fmap fst) . decimal

inputText :: Text
inputText = decodeUtf8 $(embedFile "src/input.txt")
