module Euler.Util.FrontierSearch
    ( Conf(..)
    , searchValues
    , searchNodes
    ) where

import Euler.Prelude

import Euler.Util.List (dedupe)

import qualified Data.Set as Set

------------------------------------------------------------------------

data Conf node value = Conf
    { next      :: node -> [node]
    -- ^ When node @n@ is visited, the nodes @next n@ are added to the
    -- frontier. The new nodes must have a higher 'nodeValue' than the
    -- node they came from.
    , nodeValue :: node -> value
    , start     :: [node]
    }

searchValues :: (Ord node, Ord value) => Conf node value -> [value]
searchValues = dedupe . map nodeWrapperValue . search

searchNodes :: (Ord node, Ord value) => Conf node value -> [node]
searchNodes = dedupe . map unwrap . search

search :: (Ord node, Ord value) =>
  Conf node value -> [NodeWrapper node value]
search conf = nodesFromFrontier conf initialFrontier
  where initialFrontier = Set.fromList $ nodeWrapper conf <$> start conf

data NodeWrapper node value =
  NodeWrapper node value deriving (Eq, Show)

instance (Ord node, Ord value) => Ord (NodeWrapper node value) where
  compare = compare `on` (\(NodeWrapper node value) -> (value, node))

unwrap :: NodeWrapper node value -> node
unwrap (NodeWrapper node _) = node

nodeWrapperValue :: NodeWrapper node value -> value
nodeWrapperValue (NodeWrapper _ value) = value

nodeWrapper :: Conf node value -> node -> NodeWrapper node value
nodeWrapper conf node = NodeWrapper node (nodeValue conf node)

next' :: Conf node value -> node -> [NodeWrapper node value]
next' conf node = nodeWrapper conf <$> next conf node

type Frontier node value = Set (NodeWrapper node value)

-- | As we replace nodes on the search frontier with their 'next' nodes,
-- we grow the visited space. At each step we select the frontier node
-- with the lowest value, so the 'nodes' are ordered by value ascending
-- (with ties broken arbitrarily but consistently).
nodesFromFrontier :: (Ord node, Ord value) =>
  Conf node value -> Frontier node value -> [NodeWrapper node value]
nodesFromFrontier conf frontier =
    node : nodesFromFrontier conf newFrontier
  where
    (node, frontier') = fromJust (Set.minView frontier)
    newFrontier = Set.union frontier' (Set.fromList nextNodes)
    nextNodes = next' conf (unwrap node)
