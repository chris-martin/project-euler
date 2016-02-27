module Euler.Problems.Problem46
    ( answer
    , goldbachNumbers

    -- * Nodes
    , Node
    , nodeValue
    , next
    , nodes

    -- * Miscellaneous
    , squareDoubles

    ) where

import Euler.Util.List     ( dedupe )

import Data.Function       ( on )
import Data.Numbers.Primes ( isPrime, primes )
import Data.Ord            ( comparing )
import Data.Set            ( Set )

import Data.List

import qualified Data.List.Ordered  as OL
import qualified Data.Set           as Set


-- | The smallest odd composite that cannot be written as the sum of a
-- prime and twice a square.
answer :: Integer
answer = (head . (filter (not . isPrime)))
         (OL.minus [3, 5..] goldbachNumbers)

-- | All numbers that can be written as the sum of a prime
-- and twice a square, in ascending order.
--
-- We're calling these number "goldbachs" after Christian Goldbach;
-- this is just made-up terminology here for lack of a better word.
goldbachNumbers :: Integral a => [a]
goldbachNumbers = (dedupe . (map nodeValue)) nodes


-----------------------------------------------------------------------------
--  Nodes
-----------------------------------------------------------------------------

-- | A 'Node' can be visualized as a cell in a grid.
--
-- > ..#...
-- > ..#...
-- > ..#...
-- > ..x###
-- > ......
data Node a = Node a [a] [a]

-- | The 'next' @'Node's@ are the two positions up and right, respectively,
-- on the grid. The axes in this problem are 'primes' and 'squareDoubles',
-- though that detail is irrelevant to the search strategy (other than the
-- necessity that both lists be ascending).
--
-- > ..#...  ...#..
-- > ..#...  ...#..
-- > ..1###  ...#..
-- > ..x...  ..x2##
-- > ......  ......
next :: Num a => Node a -> [Node a]
next (Node _ xs ys) = [ createNode xs (tail ys)
                      , createNode (tail xs) ys ]

-- | The value of a node is the sum of the 'head' of each list.
nodeValue :: Node a -> a
nodeValue (Node x _ _) = x

-- | As we replace nodes on the search frontier with their 'next' nodes, we grow
-- the visited space upwards and to the right. At each step we select the frontier
-- node with the lowest value, so the 'nodes' are ordered by value ascending
-- (with ties broken arbitrarily but consistently).
nodes :: Integral a => [Node a]
nodes = nodesFromFrontier initFrontier
  where
    initFrontier = Set.singleton (createNode primes squareDoubles)
    nodesFromFrontier frontier =
      case Set.minView frontier of
        Just (node, frontier') ->
          let newFrontier = Set.union frontier' (Set.fromList (next node))
          in node : (nodesFromFrontier newFrontier)

instance Show a => Show (Node a) where show    = showNode
instance Ord  a => Ord  (Node a) where compare = compare `on` nodeComparison
instance Ord  a => Eq   (Node a) where (==)    = (==)    `on` nodeComparison

-- Nodes are compared using the heads of their lists (because the lists are
-- infinite and thus nor comparable).
nodeComparison :: Node a -> (a, a, a)
nodeComparison (Node v (x:_) (y:_)) = (v, x, y)

-- Similarly, we can't derive Show because the lists are infinite.
showNode :: Show a => Node a -> String
showNode (Node v (x:_) (y:_)) =
  "Node (" ++ (show v) ++ " = " ++ (show x) ++ " + " ++ (show y) ++ ")"

createNode :: Num a => [a] -> [a] -> Node a
createNode xs ys = Node (head xs + head ys) xs ys


-----------------------------------------------------------------------------
--  Miscellaneous
-----------------------------------------------------------------------------

-- | Equivalent to @'map' (\n -> 2 * n^2) [1..]@
squareDoubles :: Integral a => [a]
squareDoubles = scanl1 (+) [2, 6 ..]
