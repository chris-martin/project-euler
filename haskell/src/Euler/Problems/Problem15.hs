module Euler.Problems.Problem15
    ( answer, countPaths, countPaths', P, Counts ) where

import Euler.Prelude

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

answer :: Integer
-- ^ The number of routes through a 20x20 grid.

answer = countPaths (20, 20)

countPaths :: P       -- ^ (x, y)
           -> Integer -- ^ The number of routes through an x-by-y grid.

countPaths d = (Maybe.fromJust . Map.lookup d) (countPaths' Map.empty [d])

type P = (Integer, Integer) -- ^ Position in the grid

type Counts = Map P Integer -- ^ Mapping from positions in the grid to
                            --   the number of routes from the position

countPaths' :: Counts -- ^ Some counts that are already known
            -> [P]    -- ^ Stack of positions yet to be considered
            -> Counts -- ^ Counts from every position

-- When the stack is empty, we're done.
countPaths' counts [] = counts

countPaths' counts stack@((x, y):restOfStack)

  -- If the stack head's count is already known, just pop it off.
  | Map.member (x, y) counts = countPaths' counts restOfStack

  -- If either coordinate is zero, there's only one path.
  -- Add it to the counts and pop it off the stack,
  | x == 0 || y == 0 = countPaths' (Map.insert (x, y) 1 counts) restOfStack

  -- If both of the adjacencies' counts are known, then this count
  -- is their sum. Add it to the counts and pop it off the stack,
  | null unknownAdjacencies =
      let c = sum $ mapMaybe (`Map.lookup` counts) adjacencies
      in  countPaths' (Map.insert (x, y) c counts) restOfStack

  -- There are some unknown adjacencies. Add them to the stack.
  | otherwise = countPaths' counts (unknownAdjacencies ++ stack)

  where
    adjacencies = [(x-1, y), (x, y-1)]
    unknownAdjacencies = filter (`Map.notMember` counts) adjacencies
