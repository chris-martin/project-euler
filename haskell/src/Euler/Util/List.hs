module Euler.Util.List
  (
  -- $setup
    neTails
  , sliding
  , transpose
  , untilNothing
  , maximumOn
  , countDistinct
  , mode
  , dedupe
  , adjustEach
  , adjustIndex
  ) where

import Euler.Prelude

import Data.List ((!!))

import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.MultiSet as MultiSet
import qualified Data.Set as Set

{- $setup

>>> import Data.Char (toUpper)

-}

---------------------------------------------------------------------------

{- |

Like 'tails', but only the non-empty tails from a non-empty list. The result is
non-empty because every non-empty list has at least one non-empy suffix
(itself).

>>> neTails (1 :| [])
(1 :| []) :| []

>>> neTails ('a' :| "bc")
('a' :| "bc") :| ['b' :| "c",'c' :| ""]

-}
neTails :: NonEmpty a -> NonEmpty (NonEmpty a)
neTails = NE.fromList . catMaybes . NE.toList . fmap NE.nonEmpty . NE.tails

{- | Sublists of a fixed length.

>>> sliding 2 "abcd"
["ab","bc","cd"]

>>> sliding 3 "abcde"
["abc","bcd","cde"]

-}
sliding :: Int -> [a] -> [[a]]
sliding _ [] = []
sliding n xs@(_:ys) =
  if List.length xs >= n
    then List.take n xs : sliding n ys
    else []

{- |

>>> transpose ["abc","def"]
["ad","be","cf"]

-}
transpose :: [[a]] -> [[a]]
transpose []     = []
transpose ([]:_) = []
transpose xs     = (List.head <$> xs) : transpose (List.tail <$> xs)

{- |

>>> untilNothing [Just 1, Just 2, Nothing, Just 3]
[1,2]

-}
untilNothing :: [Maybe a] -> [a]
untilNothing = catMaybes . List.takeWhile isJust

{- |

@'maximumOn' f@ is equivalent to @'maximumBy' ('compare' `'on'` f)@, but is more
efficient when @f@ is costly.

-}
maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn f = fst . maximumBy (compare `on` snd) . fmap (\x -> (x, f x))

{- | The number of unique elements in a list.

>>> countDistinct []
0

>>> countDistinct "aaaaabaa"
2

-}
countDistinct :: (Ord a, Integral b) => [a] -> b
countDistinct = fromIntegral . List.length . Set.fromList

{- |

The most common element in the list, assuming the list is nonempty and has a
single most common element.

>>> mode "abbbbcc"
'b'

-}
mode :: Ord a => [a] -> a
mode = fst . maximumOn snd . MultiSet.toOccurList . MultiSet.fromList

{- |

Remove consecutive duplicate elements from a list.

>>> dedupe []
[]

>>> dedupe "abbbbcca"
"abca"

-}
dedupe :: Eq a => [a] -> [a]
dedupe = fmap List.head . group

{- |

>>> adjustIndex 2 toUpper "abcd"
"abCd"

-}
adjustIndex :: Int -> (a -> a) -> [a] -> [a]
adjustIndex i f xs = List.take i xs <> [f $ xs !! i] <> List.drop (i+1) xs

{- |

>>> adjustEach toUpper "abc"
["Abc","aBc","abC"]

>>> adjustEach id []
[]

-}
adjustEach :: (a -> a) -> [a] -> [[a]]
adjustEach f xs = [ adjustIndex i f xs | i <- [0 .. List.length xs - 1] ]
