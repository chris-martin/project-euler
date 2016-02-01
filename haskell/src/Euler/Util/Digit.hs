module Euler.Util.Digit
    ( intDigits
    , textDigits
    , stringDigits
    , charIntMaybe
    ) where

import Data.Char  ( digitToInt )
import Data.Maybe ( catMaybes )
import Data.Text  ( Text, unpack )

intDigits :: (Integral a, Integral b) => a -> [b]
intDigits = (map charToInt) . show . toInteger
    where charToInt c = fromIntegral (read [c] :: Int)

textDigits :: Text -> [Int]
textDigits = stringDigits . unpack

stringDigits :: String -> [Int]
stringDigits = catMaybes . (map charIntMaybe)

charIntMaybe :: Char -> Maybe Int
charIntMaybe c
    | c `elem` ['0'..'9'] = Just $ digitToInt c
    | otherwise = Nothing
