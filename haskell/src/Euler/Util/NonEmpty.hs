module Euler.Util.NonEmpty
    ( neTails
    ) where

import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Maybe         ( catMaybes )

import qualified Data.List.NonEmpty as NE

-- Like 'tails', but only the non-empty tails from a non-empty list.
-- The result is non-empty because every non-empty list has at least
-- one non-empy suffix (itself). For example,
--
-- > neTails (NE.fromList "abc") == NE.fromList [ NE.fromList "abc"
--                                              , NE.fromList "bc"
--                                              , NE.fromList "c"]
neTails :: NonEmpty a -> NonEmpty (NonEmpty a)
neTails = NE.fromList . catMaybes . NE.toList . (fmap NE.nonEmpty) . NE.tails
