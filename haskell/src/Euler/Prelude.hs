module Euler.Prelude
  ( module X
  , (<&>)
  , showInteger, showNatural
  ) where

import Control.Applicative as X (liftA2, Applicative (..))
import Control.Arrow as X ((&&&), (>>>), (<<<))
import Control.Monad as X (Monad (..), (=<<), guard, join)
import Data.Bifunctor as X (bimap)
import Data.Bool as X (Bool (..), (&&), (||), otherwise, not)
import Data.Char as X (digitToInt, Char)
import Data.Either as X (Either (..))
import Data.Eq as X (Eq (..))
import Data.Foldable as X (fold, foldr', foldl, foldl', maximumBy, toList, Foldable, foldMap)
import Data.Function as X (id, on, flip, (&), (.), ($))
import Data.Functor as X (Functor (..), (<$>), ($>))
import Data.List as X
  (find, findIndex, group, inits, intercalate, permutations, sort)
import Data.List.NonEmpty as X (NonEmpty (..), nonEmpty)
import Data.Map as X (Map)
import Data.Maybe as X
  (catMaybes, fromJust, fromMaybe, isJust, mapMaybe, Maybe (..))
import Data.Monoid as X (Monoid (..))
import Data.Numbers.Primes as X (isPrime, primes)
import Data.Ord as X (compare, comparing, Ord (..), Ordering (..))
import Data.Ratio as X (Ratio, denominator, numerator, (%))
import Data.Semigroup as X (Semigroup (..))
import Data.Set as X (Set, (\\))
import Data.SetMap as X (SetMap)
import Data.Text as X (Text, unpack)
import Data.Traversable as X (sequenceA, traverse)
import Data.Tuple as X (fst, snd, curry, uncurry)
import Numeric.Natural as X (Natural)
import Prelude as X
  (Integer, String, Show (..), Num (..), Integral (..), fromIntegral, Int,
  RealFrac (..), (^), (^^), Floating (..), undefined, Double, even, odd, recip,
  Rational, Fractional (..))
import System.IO as X (IO)

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
infixl 1 <&>

showInteger :: Integer -> String
showInteger = show

showNatural :: Natural -> String
showNatural = show
