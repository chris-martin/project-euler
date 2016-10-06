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

import Data.Char  (digitToInt)
import Data.Maybe (catMaybes)
import Data.Text  (Text, unpack)

import qualified Data.Text.Read as TextRead

-- $setup
-- >>> import Test.QuickCheck

-----------------------------------------------------------------

-- | Digits of a positive integer.
digits :: (Integral a, Integral b)
    => b   -- ^ Base
    -> a   -- ^ Number to convert
    -> [b]

-- | Digits of a positive integer as a list, in reverse order.
-- This is slightly more efficient than in forward order.
digitsRev :: (Integral a, Integral b)
    => b   -- ^ Base
    -> a   -- ^ Number to convert
    -> [b]

-- | The positive integer represented by a list of digits,
unDigits :: (Integral a, Integral b)
    => b   -- ^ Base
    -> [b] -- ^ Digits
    -> a

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

intPalindrome :: (Integral a, Integral b)
    => b    -- ^ Base
    -> a    -- ^ Number to test
    -> Bool
-- ^ @'intPalindrome' b n@ indicates whether the base-/b/
-- representation /n/ is a palindrome.
--
-- >>> intPalindrome 10 <$> [33, 13431, 21, 335]
-- [True,True,False,False]
--
-- Zero and one are palindromes in any base.
--
-- prop> \(Positive n) -> intPalindrome (n+1) 0
-- prop> \(Positive n) -> intPalindrome (n+1) 1

-----------------------------------------------------------------

digitsRev _ 0 = []
digitsRev base i = let (rest, lastDigit) = quotRem i (fromIntegral base)
                   in  (fromIntegral lastDigit) : digitsRev base rest

digits base = reverse . digitsRev base

unDigits base = foldl f 0
  where f a b = a * (fromIntegral base) + (fromIntegral b)

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
