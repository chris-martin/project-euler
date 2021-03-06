module Euler.Util.Foldable
  ( allEqual
  ) where

import Euler.Prelude

{- |

Whether all of the items in a list are equal.

-- >>> allEqual <$> [[], [1], [1,1], [1,2]]
-- [True,True,True,False]

-}
allEqual :: (Foldable f, Eq a) => f a -> Bool
allEqual = foldMap Aeq_same >>> not . aeq_different

------------------------------------------------------------------------

data Aeq a = Aeq_nothing   -- ^ Empty
           | Aeq_different -- ^ Contains different elements
           | Aeq_same a    -- ^ Contains only this element

instance Eq a => Semigroup (Aeq a)
  where
    (<>) (Aeq_same x) (Aeq_same y)
            | x == y    = Aeq_same x
            | otherwise = Aeq_different

    (<>) Aeq_different _ = Aeq_different
    (<>) _ Aeq_different = Aeq_different

    (<>) Aeq_nothing x = x
    (<>) x Aeq_nothing = x

instance (Eq a) => Monoid (Aeq a)
  where
    mempty = Aeq_nothing

aeq_different :: Aeq a -> Bool

aeq_different Aeq_different = True
aeq_different _             = False
