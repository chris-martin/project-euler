module Euler.Util.Digit
    ( textDigits
    , stringDigits
    , charIntMaybe
    ) where

import Data.Char  ( digitToInt )
import Data.Maybe ( catMaybes )
import Data.Text  ( Text, unpack )

textDigits :: Text -> [Int]
textDigits = stringDigits . unpack

stringDigits :: String -> [Int]
stringDigits = catMaybes . (map charIntMaybe)

charIntMaybe :: Char -> Maybe Int
charIntMaybe c
    | c `elem` ['0'..'9'] = Just $ digitToInt c
    | otherwise = Nothing
