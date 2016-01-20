{-# LANGUAGE TemplateHaskell #-}

module Problem8 where

import Data.Char (digitToInt)
import Data.FileEmbed (embedFile)
import Data.Maybe (catMaybes)
import Data.Text.Encoding (decodeUtf8)

answer :: Integer
answer = maximum $ map product $ sliding 5 $ map toInteger digits
  where

    sliding n xs
      | length xs >= n = (take n xs) : (sliding n $ tail xs)
      | otherwise = []

    digits = catMaybes $ map charToIntMaybe $ show inputText

    charToIntMaybe c
      | c `elem` ['0'..'9'] = Just $ digitToInt c
      | otherwise = Nothing

    inputText = decodeUtf8 $(embedFile "src/input.txt")
