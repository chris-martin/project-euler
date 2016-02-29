module Euler.Util.Digit
    (
    -- $setup

    -- * Conversions to list of digits
      digits, textDigits, stringDigits

    -- * Conversions to single digits
    , charIntMaybe

    -- * Conversions from digits to integers
    , textIntMaybe, unDigits

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

-- $setup
-- >>> import Test.QuickCheck

-----------------------------------------------------------------

stringDigits :: String -> [Int]
-- ^ Get the values of numeric characters from a string, ignoring any
-- non-numeric characters.
--
-- >>> stringDigits " 0 x0 42a"
-- [0,0,4,2]

textDigits :: Text -> [Int]
-- ^ Same as 'stringDigits', for 'Text' instead of 'String'.

charIntMaybe  :: Char -> Maybe Int
-- ^ Maps the ten numeric ascii characters to their numeric values,
-- and all other characters to 'Nothing'.
--
-- >>> charIntMaybe '0'
-- Just 0
-- >>> charIntMaybe '9'
-- Just 9
-- >>> charIntMaybe 'a'
-- Nothing

textIntMaybe  :: Integral a => Text -> Maybe a

intPalindrome :: Integral a => a -> a -> Bool
-- ^ @'intPalindrome' b n@ indicates whether the base-/b/
-- representation /n/ is a palindrome.
--
-- >>> intPalindrome 10 33
-- True
-- >>> intPalindrome 10 13431
-- True
-- >>> intPalindrome 10 21
-- False
-- >>> intPalindrome 10 335
-- False
--
-- Zero and one are palindromes in any base.
--
-- todo - change (n+1) to n - base 1 is broken. https://github.com/chris-martin/project-euler/issues/8
--
-- prop> \(Positive n) -> intPalindrome (n+1) 0
-- prop> \(Positive n) -> intPalindrome (n+1) 1

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
