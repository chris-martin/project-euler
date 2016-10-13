module Euler.Problems.Problem68 (answer) where

import Data.Function ((&))
import Data.List     (permutations)

type Group = [Int]
type Ring = [Group]

answer :: String
answer = maximum $ concat . (fmap $ concat . fmap show) <$> magicRings

magicRings :: [Ring]
magicRings = filter isMagic rings

isMagic :: Ring -> Bool
isMagic = allSame . fmap sum

rings :: [Ring]
rings = do
    p <- (10:) <$> permutations [1..9]
    return $ indices & fmap (fmap (p !!)) & cycleToMin

-- Groups from the diagram below. The starting point is arbitrary,
-- but it's important that the groups are in clockwise order.
indices :: Ring
indices = [ [ 0, 5, 6 ]
          , [ 1, 6, 7 ]
          , [ 2, 7, 8 ]
          , [ 3, 8, 9 ]
          , [ 4, 9, 5 ]
          ]
{-

The string is 16 digits iff the 10 is on an external node.
So we fix the 10 at index 0, an arbitrary point on the outer ring.

The indices in this diagram are arbitrary, except for 0; since it
represents 10, it must be on the outer ring.

       0
        \
         5     1
        /  \  /
       9    6
      /\   /
     4  8-7--2
         \
          3

-}

-- Rotate the circle around such that the minimum element is first.
cycleToMin :: Ord a => [a] -> [a]
cycleToMin xs = drop i xs ++ take i xs
  where i = indexOfMin xs

-- The index of the minimum element in the list.
indexOfMin :: Ord a => [a] -> Int
indexOfMin xs = zip xs [0..] & minimum & snd

-- Whether all of the items in a list are equal.
allSame :: Eq a => [a] -> Bool
allSame []  = True
allSame [x] = True
allSame (x : zs@(y : _)) = x == y && allSame zs
