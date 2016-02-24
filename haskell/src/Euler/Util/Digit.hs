module Euler.Util.Digit
    (
    -- * Conversions to list of digits
      textDigits, stringDigits

    -- * Conversions to single digits
    , charIntMaybe

    -- * Conversions from digits to integers
    , textIntMaybe

    -- * Other stuff
    , intPalindrome
    ) where

import Data.Char   ( digitToInt )
import Data.Digits ( digits, digitsRev, unDigits )
import Data.List   ( reverse )
import Data.Maybe  ( catMaybes )
import Data.Text   ( Text, unpack )

import qualified Data.Char as Char
import qualified Data.Text.Read  as TextRead

-----------------------------------------------------------------

textDigits :: Text -> [Int]

stringDigits :: String -> [Int]

-- | Maps the ten numeric ascii characters to their numeric values
-- (@\'0\'@ to @0@, @\'1\'@ to @1@, ..., @\'9\'@ to @9@), and all
-- other characters to 'Nothing'.
charIntMaybe  :: Char -> Maybe Int

textIntMaybe  :: Integral a => Text -> Maybe a

-- | Whether an integer's decimal representation is a palindrome.
-- For example, 0, 33, and 13431 are palindromes; 21 and 335 are not.
intPalindrome :: Integral a => a -> a -> Bool

-----------------------------------------------------------------

textDigits = stringDigits . unpack

textIntMaybe = do e <- TextRead.decimal
                  return $ case e of Right(a, t) -> Just a
                                     _           -> Nothing

stringDigits = catMaybes . (map charIntMaybe)

charIntMaybe c
    | c `elem` ['0'..'9'] = Just $ digitToInt c
    | otherwise = Nothing

intPalindrome b n = let ds = digitsRev b n
                    in  ds == reverse ds
