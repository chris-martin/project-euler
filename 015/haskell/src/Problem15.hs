module Problem15 (answer) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)

type Point = (Int, Int)

type Counts = Map Point Integer

countPaths :: Counts -> [Point] -> Counts

-- When the stack is empty, we're done.
countPaths counts [] = counts

countPaths counts stack@((x, y):restOfStack)

    -- If the stack head's count is already known, just pop it off.
    | Map.member (x, y) counts = countPaths counts restOfStack

    -- If either coordinate is zero, there's only one path.
    -- Add it to the counts and pop it off the stack,
    | x == 0 || y == 0 = countPaths (Map.insert (x, y) 1 counts) restOfStack

    -- If both of the adjacencies' counts are known, then this count
    -- is their sum. Add it to the counts and pop it off the stack,
    | null unknownAdjacencies =
        let c = sum $ catMaybes $ map (flip Map.lookup counts) adjacencies
        in countPaths (Map.insert (x, y) c counts) restOfStack

    -- There are some unknown adjacencies. Add them to the stack.
    | otherwise = countPaths counts (unknownAdjacencies ++ stack)

    where adjacencies = [ ( x - 1 , y     )
                        , ( x     , y - 1 ) ]
          unknownAdjacencies = filter (flip Map.notMember counts) adjacencies

answer :: Integer
answer = fromJust $ Map.lookup d $ countPaths Map.empty [d]
    where d = (20, 20)
