module Euler.Util.Foldable
    ( allEqual
    ) where

allEqual :: (Foldable f, Eq a) => f a -> Bool
-- ^ Whether all of the items in a list are equal.
--
-- >>> allEqual <$> [[], [1], [1,1], [1,2]]
-- [True,True,True,False]

------------------------------------------------------------------------

data Aeq a = Aeq_nothing   -- ^ Empty
           | Aeq_different -- ^ Contains different elements
           | Aeq_same a    -- ^ Contains only this element

instance (Eq a) => Monoid (Aeq a) where

    mempty = Aeq_nothing

    mappend (Aeq_same x) (Aeq_same y) | x == y    = Aeq_same x
                                      | otherwise = Aeq_different

    mappend Aeq_different _ = Aeq_different
    mappend _ Aeq_different = Aeq_different

    mappend Aeq_nothing x = x
    mappend x Aeq_nothing = x

aeq_different :: Aeq a -> Bool

aeq_different Aeq_different = True
aeq_different _             = False

allEqual xs = not . aeq_different $ foldMap Aeq_same xs
