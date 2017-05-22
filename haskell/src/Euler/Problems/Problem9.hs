{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}

module Euler.Problems.Problem9
    ( answer
    , answerTriple
    ) where

import Euler.Prelude

data Triple a = Triple a a a
  deriving (Eq, Ord, Show, Functor, Foldable)

answer :: Integer
answer = product answerTriple

-- | The pythagorean triple the question asks us to find.
-- The tuple /(a, b, c)/ is ordered such that /a < b < c/.
--
-- >>> answerTriple
-- Triple 200 375 425
answerTriple :: Integral a => Triple a
answerTriple = (threeSort . head) (filter isPythagorean tripleCandidates)

-- | Candidate triples that aren't necessarily pythagorean
tripleCandidates :: Integral a => [Triple a]
tripleCandidates =
    completeTriple <$> pairsOf [1 .. magicNumber]
  where
    completeTriple (a, b) = Triple a b (magicNumber - a - b)
    magicNumber = 1000

isPythagorean :: Integral a => Triple a -> Bool
isPythagorean (Triple a b c) =
  a * a + b * b == c * c

pairsOf :: [a] -> [(a, a)]
pairsOf xs =
  xs >>= \a -> xs <&> \b -> (a, b)

threeSort :: Ord a => Triple a -> Triple a
threeSort (Triple x y z) =
  case sort [x, y, z] of [x', y', z'] -> Triple x' y' z'
