module Euler.Util.Digit
    (
    -- * Conversions to list of digits
      textDigits, stringDigits

    -- * Conversions to single digits
    , charIntMaybe

    -- * Other stuff
    , intPalindrome
    ) where

import Data.Char   ( digitToInt )
import Data.Digits ( digits, digitsRev, unDigits )
import Data.List   ( reverse )
import Data.Maybe  ( catMaybes )
import Data.Text   ( Text, unpack )

import qualified Data.Char as Char

textDigits    :: Text   -> [Int]
stringDigits  :: String -> [Int]
charIntMaybe  :: Char   -> Maybe Int
intPalindrome :: Integral a => a -> a -> Bool

-----------------------------------------------------

textDigits = stringDigits . unpack

stringDigits = catMaybes . (map charIntMaybe)

charIntMaybe c
    | c `elem` ['0'..'9'] = Just $ digitToInt c
    | otherwise = Nothing

intPalindrome b n = let ds = digitsRev b n
                    in  ds == reverse ds
