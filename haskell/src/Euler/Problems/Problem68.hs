{- |

The string is 16 digits iff the 10 is on an external node.
So we fix the 10 at index 0, an arbitrary external node.

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
module Euler.Problems.Problem68
  ( answer
  ) where

import Euler.Prelude

import Euler.Util.Foldable (allEqual)

import Data.List (filter, (!!))

import qualified Data.Foldable as Foldable
import qualified Data.List as List

type Group = [Int]
type Ring = [Group]

answer :: String
answer =
  Foldable.maximum $ foldMap (foldMap show) <$> magicRings

magicRings :: [Ring]
magicRings =
  filter isMagic rings

isMagic :: Ring -> Bool
isMagic =
  allEqual . fmap Foldable.sum

{- |

All rings that produce 16-digit strings, possibly including repetition.

-}
rings :: [Ring]
rings =
  (10:) <$> permutations [1..9] <&> \p ->
  indices & fmap (fmap (p !!)) & cycleToMin

{- |

Groups from the diagram above. The starting point is arbitrary, but it's
important that the groups are in clockwise order.

-}
indices :: Ring
indices =
  [ [ 0, 5, 6 ]
  , [ 1, 6, 7 ]
  , [ 2, 7, 8 ]
  , [ 3, 8, 9 ]
  , [ 4, 9, 5 ]
  ]

{- |

Rotate the circle around such that the minimum element is first.

-}
cycleToMin :: Ord a => [a] -> [a]
cycleToMin xs =
  let i = indexOfMin xs
  in  List.drop i xs <> List.take i xs

{- |

The index of the minimum element in the list.

-}
indexOfMin :: Ord a => [a] -> Int
indexOfMin xs =
  List.zip xs [0..] & Foldable.minimum & snd
