module Euler.Prelude
  ( module X
  , (<&>)
  , showInteger
  ) where

import Control.Applicative as X (liftA2)
import Control.Arrow as X ((&&&))
import Control.Monad as X (guard, join)
import Data.Bifunctor as X (bimap)
import Data.Char as X (digitToInt)
import Data.Foldable as X (foldr', maximumBy, toList)
import Data.Function as X (on, (&))
import Data.List as X
  (find, findIndex, group, inits, intercalate, permutations, sort)
import Data.List.NonEmpty as X (NonEmpty (..), nonEmpty)
import Data.Map as X (Map)
import Data.Maybe as X (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import Data.Monoid as X ((<>))
import Data.Numbers.Primes as X (isPrime, primes)
import Data.Ord as X (compare, comparing)
import Data.Ratio as X (Ratio, denominator, numerator, (%))
import Data.Set as X (Set, (\\))
import Data.SetMap as X (SetMap)
import Data.Text as X (Text, unpack)

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
infixl 1 <&>

showInteger :: Integer -> String
showInteger = show
