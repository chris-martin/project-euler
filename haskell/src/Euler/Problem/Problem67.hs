{-# LANGUAGE TemplateHaskell #-}

module Euler.Problem.Problem67 (answer) where

import Data.FileEmbed     ( embedFile )
import Data.Text          ( Text )
import Data.Text.Encoding ( decodeUtf8 )

import Euler.Util.TrianglePath

answer :: Int
answer = reduceTriangle $ parseTriangle $ text

text :: Text
text = decodeUtf8 $(embedFile "../problems/67-data.txt")
