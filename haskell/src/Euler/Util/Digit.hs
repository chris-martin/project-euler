{-# LANGUAGE LambdaCase #-}

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

import Euler.Prelude

import qualified Data.List as List
import qualified Data.Text.Read as TextRead

{- $setup

>>> import Test.QuickCheck

-}

{- |

Digits of a positive integer.

-}
digits :: (Integral a, Integral b)
  => b   -- ^ Base
  -> a   -- ^ Number to convert
  -> [b]
digits base = digitsRev base >>> List.reverse

{- |

Digits of a positive integer as a list, in reverse order. This is slightly more
efficient than in forward order.

-}
digitsRev :: (Integral a, Integral b)
  => b   -- ^ Base
  -> a   -- ^ Number to convert
  -> [b]
digitsRev _ 0 = []
digitsRev base i = let (rest, lastDigit) = quotRem i (fromIntegral base)
                   in  fromIntegral lastDigit : digitsRev base rest

{- |

The positive integer represented by a list of digits,

-}
unDigits :: (Integral a, Integral b)
  => b   -- ^ Base
  -> [b] -- ^ Digits
  -> a
unDigits base = foldl' f 0
  where f a b = a * fromIntegral base + fromIntegral b

{- |

Get the values of numeric characters from a string, ignoring any non-numeric
characters.

>>> stringDigits " 0 x0 42a"
[0,0,4,2]

-}
stringDigits :: String -> [Int]
stringDigits = mapMaybe charIntMaybe

{- |

Same as 'stringDigits', for 'Text' instead of 'String'.

-}
textDigits :: Text -> [Int]
textDigits = stringDigits . unpack

textIntMaybe  :: Integral a => Text -> Maybe a
textIntMaybe =
  TextRead.decimal <&>
  \case
    Right (a, _) -> Just a
    _            -> Nothing

{- |

Maps the ten numeric ascii characters to their numeric values, and all other
characters to 'Nothing'.

>>> charIntMaybe '0'
Just 0

>>> charIntMaybe '9'
Just 9

>>> charIntMaybe 'a'
Nothing

-}
charIntMaybe :: Char -> Maybe Int
charIntMaybe c =
  guard (List.elem c ['0'..'9']) $> digitToInt c

{- |

@'intPalindrome' b n@ indicates whether the base-/b/ representation /n/ is a
palindrome.

>>> intPalindrome 10 <$> [33, 13431, 21, 335]
[True,True,False,False]

Zero and one are palindromes in any base.

prop> \(Positive n) -> intPalindrome (n+1) 0
prop> \(Positive n) -> intPalindrome (n+1) 1

-}
intPalindrome :: (Integral a, Integral b)
  => b    -- ^ Base
  -> a    -- ^ Number to test
  -> Bool
intPalindrome b n =
  let ds = digitsRev b n
  in  ds == List.reverse ds
