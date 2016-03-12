module Euler.Util.Inf
    ( Inf
    , fromList
    , toList
    ) where

import           Data.Function      (on)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

newtype Inf a = Inf (NonEmpty a)
-- ^ A wrapper for infinite lists, with typeclasses operating
-- on only the list's 'head'. This is not correct in all
-- circumstances, but is useful for (for example) tails of
-- an infinite sequence without duplicate elements.

instance Show a => Show (Inf a) where
  show (Inf (x :| _)) = "Inf (" ++ show x ++ ", ...)"
-- ^ >>> show $ fromList [5..]
-- "Inf (5, ...)"

instance Ord a => Ord (Inf a) where
  compare = compare `on` infHead

instance Eq a => Eq (Inf a) where
  (==) = (==) `on` infHead
-- ^ prop> fromList [5..] == fromList [5..]
-- prop> fromList [5..] /= fromList [6..]

fromList :: [a] -> Inf a
fromList xs = Inf $ NE.fromList xs

toList :: Inf a -> [a]
toList (Inf xs) = NE.toList xs

infHead :: Inf a -> a
infHead (Inf (x :| _)) = x
