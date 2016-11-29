module Euler.Problems.Problem9
    ( answer
    , answerTriple
    ) where

import Euler.Prelude

----------------------------------------------------------------------

answer :: Integer

answerTriple :: Integral a => (a, a, a)
-- ^ The pythagorean triple the question asks us to find.
-- The tuple /(a, b, c)/ is ordered such that /a < b < c/.
--
-- >>> answerTriple
-- (200,375,425)

----------------------------------------------------------------------

answer = threeProduct answerTriple

answerTriple = (threeSort . head) (filter isPythagorean tripleCandidates)

-- | Candidate triples that aren't necessarily pythagorean
tripleCandidates :: Integral a => [(a, a, a)]
tripleCandidates = map completeTriple $ pairsOf [1 .. magicNumber]
  where
    completeTriple (a, b) = (a, b, magicNumber - a - b)
    magicNumber = 1000

isPythagorean :: Integral a => (a, a, a) -> Bool
isPythagorean (a, b, c) = a*a + b*b == c*c

pairsOf :: [a] -> [(a, a)]
pairsOf xs = do a <- xs; b <- xs; return (a, b)

threeProduct :: Num a => (a, a, a) -> a
threeProduct (x, y, z) = x * y * z

threeSort :: Ord a => (a, a, a) -> (a, a, a)
threeSort (x, y, z) = case sort [x, y, z] of [x', y', z'] -> (x', y', z')
