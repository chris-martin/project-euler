module Euler.Util.List
    ( neTails
    , sliding
    , transpose
    , untilNothing
    , maximumOn
    , countDistinct
    , mode
    , dedupe
    ) where

import Data.Foldable      ( maximumBy )
import Data.Function      ( on )
import Data.List          ( group )
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Maybe         ( catMaybes, isJust )
import Data.Ord           ( compare )

import qualified Data.List.NonEmpty as NE
import qualified Data.MultiSet      as MultiSet
import qualified Data.Set           as Set

---------------------------------------------------------------------------

neTails :: NonEmpty a -> NonEmpty (NonEmpty a)
-- ^ Like 'tails', but only the non-empty tails from a non-empty list.
-- The result is non-empty because every non-empty list has at least
-- one non-empy suffix (itself).
--
-- >>> neTails (1 :| [])
-- (1 :| []) :| []
--
-- >>> neTails ('a' :| "bc")
-- ('a' :| "bc") :| ['b' :| "c",'c' :| ""]

sliding :: Int -> [a] -> [[a]]
-- ^ Sublists of a fixed length.
--
-- >>> sliding 2 "abcd"
-- ["ab","bc","cd"]
--
-- >>> sliding 3 "abcde"
-- ["abc","bcd","cde"]

transpose :: [[a]] -> [[a]]
-- ^ >>> transpose ["abc","def"]
-- ["ad","be","cf"]

untilNothing :: [Maybe a] -> [a]
-- ^ >>> untilNothing [Just 1, Just 2, Nothing, Just 3]
-- [1,2]

maximumOn :: Ord b => (a -> b) -> [a] -> a
-- ^ @'maximumOn' f@ is equivalent to @'maximumBy' ('compare' `'on'` f)@,
-- but is more efficient when @f@ is costly.

countDistinct :: (Ord a, Integral b) => [a] -> b
-- ^ The number of unique elements in a list.
--
-- >>> countDistinct []
-- 0
--
-- >>> countDistinct "aaaaabaa"
-- 2

mode :: Ord a => [a] -> a
-- ^ The most common element in the list, assuming the list is nonempty and
-- has a single most common element.
--
-- >>> mode "abbbbcc"
-- 'b'

dedupe :: Eq a => [a] -> [a]
-- ^ Remove consecutive duplicate elements from a list.
--
-- >>> dedupe []
-- []
--
-- >>> dedupe "abbbbcca"
-- "abca"

---------------------------------------------------------------------------

neTails = NE.fromList . catMaybes . NE.toList . fmap NE.nonEmpty . NE.tails

sliding n xs
    | length xs >= n = (take n xs) : (sliding n $ tail xs)
    | otherwise      = []

transpose []     = []
transpose ([]:_) = []
transpose xs     = (map head xs) : transpose (map tail xs)

untilNothing = catMaybes . takeWhile isJust

maximumOn f = fst . maximumBy (compare `on` snd) . map (\x -> (x, f x))

countDistinct = fromIntegral . length . Set.fromList

mode = fst . maximumOn snd . MultiSet.toOccurList . MultiSet.fromList

dedupe = map head . group
