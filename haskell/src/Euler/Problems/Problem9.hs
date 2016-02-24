module Euler.Problems.Problem9
    ( answer
    , answerTriple
    ) where

import Data.List (sort)

----------------------------------------------------------------------

answer :: Integer

-- | The pythagorean triple the question asks us to find.
-- The tuple /(a, b, c)/ is ordered such that /a < b < c/.
answerTriple :: Integral a => Three a

----------------------------------------------------------------------

answer = threeProduct answerTriple

type Two a   = (a, a)
type Three a = (a, a, a)

answerTriple = (threeSort . head) (filter isPythagorean tripleCandidates)

-- | Candidate triples that aren't necessarily pythagorean
tripleCandidates :: Integral a => [Three a]
tripleCandidates = map completeTriple (pairsOf [1..magicNumber])
  where
    completeTriple (a, b) = (a, b, magicNumber - a - b)
    magicNumber = 1000

isPythagorean :: Integral a => Three a -> Bool
isPythagorean (a, b, c) = a*a + b*b == c*c

pairsOf :: [a] -> [Two a]
pairsOf xs = do a <- xs; b <- xs; return (a, b)

threeProduct :: Num a => Three a -> a
threeProduct (x, y, z) = x * y * z

threeSort :: Ord a => Three a -> Three a
threeSort (x, y, z) = case sort [x, y, z] of [x', y', z'] -> (x', y', z')
