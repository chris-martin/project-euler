{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}

module Euler.Problems.Problem9
  ( answer
  , answerTriple
  ) where

import Euler.Prelude

import qualified Data.Foldable as Foldable
import qualified Data.List as List

data Triple a = Triple a a a
  deriving (Eq, Ord, Show, Functor, Foldable)

answer :: Natural
answer = Foldable.product answerTriple

-- | The pythagorean triple the question asks us to find.
-- The tuple /(a, b, c)/ is ordered such that /a < b < c/.
--
-- >>> answerTriple
-- Triple 200 375 425
answerTriple :: Triple Natural
answerTriple =
  tripleCandidates & List.filter isPythagorean & List.head &
  fmap fromIntegral & threeSort

-- | Candidate triples that aren't necessarily pythagorean
tripleCandidates :: [Triple Integer]
tripleCandidates =
    completeTriple <$> pairsOf [1 .. magicNumber]
  where
    completeTriple (a, b) = Triple a b (magicNumber - a - b)
    magicNumber = 1000

isPythagorean :: Triple Integer -> Bool
isPythagorean (Triple a b c) =
  a * a + b * b == c * c

pairsOf :: [a] -> [(a, a)]
pairsOf xs =
  xs >>= \a -> xs <&> \b -> (a, b)

threeSort :: Ord a => Triple a -> Triple a
threeSort =
  Foldable.toList >>>
  \[x', y', z'] -> Triple x' y' z'
