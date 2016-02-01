{-# LANGUAGE TemplateHaskell #-}

module Euler.Problem.Problem8 (answer) where

import Data.FileEmbed     ( embedFile )
import Data.Text.Encoding ( decodeUtf8 )
import Data.Text          ( Text )

import Euler.Util.Digit   ( textDigits )

answer :: Integer
answer = maximum $ map product $ sliding 5 $ map toInteger digits
  where
    sliding n xs
      | length xs >= n = (take n xs) : (sliding n $ tail xs)
      | otherwise      = []
    digits = textDigits inputText

inputText :: Text
inputText = decodeUtf8 $(embedFile "../problems/8-data.txt")
