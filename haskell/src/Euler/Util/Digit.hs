module Euler.Util.Digit
    (
    -- * Conversions to list of digits
      intDigits, textDigits, stringDigits

    -- * Conversions from a list of digits
    , digitsInt

    -- * Conversions to single digits
    , charIntMaybe

    ) where

import Data.Char  ( digitToInt )
import Data.Maybe ( catMaybes )
import Data.Text  ( Text, unpack )

intDigits    :: (Integral a, Integral b) => a -> [b]
textDigits   :: Text   -> [Int]
stringDigits :: String -> [Int]
digitsInt    :: (Integral a, Integral b) => [a] -> b

-----------------------------------------------------

intDigits = (map charToInt) . show . toInteger where
    charToInt c = fromIntegral (read [c] :: Int)

textDigits = stringDigits . unpack

stringDigits = catMaybes . (map charIntMaybe)

charIntMaybe :: Char -> Maybe Int
charIntMaybe c
    | c `elem` ['0'..'9'] = Just $ digitToInt c
    | otherwise = Nothing

digitsInt ds = fromInteger ((read s) :: Integer) where
    s = concat $ map (\i -> show ((fromIntegral i) :: Int)) ds
