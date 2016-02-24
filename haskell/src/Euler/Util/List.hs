module Euler.Util.List
    ( neTails
    , sliding
    , transpose
    , untilNothing
    , maximumOn
    , countDistinct
    , mode
    ) where

import Data.Foldable      ( maximumBy )
import Data.Function      ( on )
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Maybe         ( catMaybes, isJust )
import Data.Ord           ( compare )

import qualified Data.List.NonEmpty as NE
import qualified Data.MultiSet      as MultiSet
import qualified Data.Set           as Set

-- | Like 'tails', but only the non-empty tails from a non-empty list.
-- The result is non-empty because every non-empty list has at least
-- one non-empy suffix (itself). For example,
--
-- > neTails (NE.fromList "abc") == NE.fromList [ NE.fromList "abc"
--                                              , NE.fromList "bc"
--                                              , NE.fromList "c"]
neTails :: NonEmpty a -> NonEmpty (NonEmpty a)
neTails = NE.fromList . catMaybes . NE.toList . (fmap NE.nonEmpty) . NE.tails

sliding :: Int -> [a] -> [[a]]
sliding n xs
    | length xs >= n = (take n xs) : (sliding n $ tail xs)
    | otherwise      = []

transpose :: [[a]] -> [[a]]
transpose []     = []
transpose ([]:_) = []
transpose xs     = (map head xs) : transpose (map tail xs)

untilNothing :: [Maybe a] -> [a]
untilNothing = catMaybes . (takeWhile isJust)

-- | @'maximumOn' f@ is equivalent to @'maximumBy' ('compare' `'on'` f)@,
-- but is more efficient when @f@ is costly.
maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn f = fst . (maximumBy (compare `on` snd)) . (map (\x -> (x, f x)))

countDistinct :: Ord a => [a] -> Int
countDistinct = length . Set.fromList

-- | The most common element in the list, assuming the list is nonempty and
-- has a single most common element.
mode :: Ord a => [a] -> a
mode = fst . (maximumOn snd) . MultiSet.toOccurList . MultiSet.fromList
