{- |

English-language representations of integers, used by Euler problem 17.

-}

{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Euler.Util.NumberWords
  ( word
  , NumberWordException (..)
  , LetterCount, getLetterCount
  ) where

import Euler.Prelude

import Control.Exception
import Control.Monad.Catch (MonadThrow (..))
import Data.Char (isAlpha)
import Data.String (IsString (..))
import Data.Semigroup (Sum (..))

import qualified Data.List as List

--------------------------------------------------------------------------------

data NumberWordException = WordUnknown Natural
  deriving (Eq, Ord, Show)

instance Exception NumberWordException

--------------------------------------------------------------------------------

newtype LetterCount = LetterCount (Sum Natural)
 deriving (Eq, Ord, Monoid, Num, Semigroup, Show)

instance IsString LetterCount
  where
    fromString =
      List.filter isAlpha >>> List.length >>> fromIntegral

getLetterCount :: LetterCount -> Natural
getLetterCount (LetterCount x) = getSum x

--------------------------------------------------------------------------------

{- |

>>> word 21 :: Maybe String
Just "twenty-one"

>>> word 300 :: Maybe String
Just "three hundred"

>>> word 645 :: Maybe String
Just "six hundred and forty-five"

-}
word :: (MonadThrow m, IsString w, Semigroup (m w)) => Natural -> m w

word  0 = pure "zero"
word  1 = pure "one"
word  2 = pure "two"
word  3 = pure "three"
word  4 = pure "four"
word  5 = pure "five"
word  6 = pure "six"
word  7 = pure "seven"
word  8 = pure "eight"
word  9 = pure "nine"
word 10 = pure "ten"
word 11 = pure "eleven"
word 12 = pure "twelve"
word 13 = pure "thirteen"
word 14 = pure "fourteen"
word 15 = pure "fifteen"
word 16 = pure "sixteen"
word 17 = pure "seventeen"
word 18 = pure "eighteen"
word 19 = pure "nineteen"
word 20 = pure "twenty"
word 30 = pure "thirty"
word 40 = pure "forty"
word 50 = pure "fifty"
word 60 = pure "sixty"
word 70 = pure "seventy"
word 80 = pure "eighty"
word 90 = pure "ninety"

word i | i < 100 =
  let (a, b) = quotRem i 10
  in  word (10 * a) <> pure "-" <> word b

word i | i < 1000 =
  case quotRem i 100 of
    (a, 0) -> word a <> pure " hundred"
    (a, b) -> word a <> pure " hundred and " <> word b

word 1000 = pure "one thousand"

word n = throw (WordUnknown n)
