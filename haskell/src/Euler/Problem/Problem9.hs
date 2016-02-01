module Euler.Problem.Problem9 (answer) where

import Data.Foldable (foldMap)

answer :: Integer
answer = tupleProduct triple
  where tupleProduct (a, b, c) = a * b * c

-- the pythagorean triple the question asks us to find
triple :: (Integer, Integer, Integer)
triple = head $ filter isPythagorean triples
  where
    -- candidate triples that aren't necessarily pythagorean
    triples = map completeTriple $ pairsOf [1..magicNumber]
    completeTriple (a, b) = (a, b, magicNumber - a - b)
    magicNumber = 1000

isPythagorean :: (Integer, Integer, Integer) -> Bool
isPythagorean (a, b, c) = a*a + b*b == c*c

pairsOf :: [a] -> [(a, a)]
pairsOf xs = foldMap (\a -> map (\b -> (a, b)) xs) xs
