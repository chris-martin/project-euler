module Euler.Util.ContinuedFractions
    ( ContinuedFraction
    , continuedFractionConvergent
    , sqrtContinuedFraction
    , sqrtConvergents
    ) where

import Euler.Prelude

import Euler.Util.Arithmetic (floorSqrt, isSquare, square)

import qualified Data.List as List

--------------------------------------------------------------------------------

type ContinuedFraction a = (a, [a])

-- https://en.wikipedia.org/w/index.php?title=Methods_of_computing_square_roots&oldid=742169687#Algorithm

{- |

>>> fmap (take 6) <$> sqrtContinuedFraction 2
Just (1,[2,2,2,2,2,2])

>>> fmap (take 14) <$> sqrtContinuedFraction 114
Just (10,[1,2,10,2,1,20,1,2,10,2,1,20,1,2])

>>> sqrtContinuedFraction 16
Nothing

-}
continuedFractionConvergent :: Integral a
  => ContinuedFraction a -- ^ Must be finite
  -> Ratio a             -- ^ The approximation represented by the
                         --   continued fraction
sqrtContinuedFraction :: Integral a
  => a                           -- ^ /n/
  -> Maybe (ContinuedFraction a) -- ^ Continued fraction expansion of
                                 -- /sqrt(n)/ in canonical form.
sqrtContinuedFraction s =
  if isSquare s
    then Nothing
    else Just (a0, List.tail $ f 0 1 a0)
  where
    a0 = floorSqrt s
    f m d a = a : f m' d' a'
      where
        m' = d * a - m
        d' = (s - square m') `div` d
        a' = (a0 + m') `div` d'

continuedFractionConvergent (a0, as) =
    (fromIntegral a0) + foldr' f 0 as
  where
    f a acc = recip $ (fromIntegral a) + acc

{- |

>>> :{
showRatio :: (Integral a, Show a) => Ratio a -> String
showRatio x = intercalate "/" $ show . ($ x) <$> [ numerator, denominator ]
:}

>>> (intercalate ", ") . (fmap showRatio) . (take 5) <$> sqrtConvergents 2
Just "1/1, 3/2, 7/5, 17/12, 41/29"

>>> (intercalate ", ") . (fmap showRatio) . (take 5) <$> sqrtConvergents 3
Just "1/1, 2/1, 5/3, 7/4, 19/11"

>>> sqrtConvergents 16
Nothing

-}
sqrtConvergents :: (Integral a)
  => a               -- ^ /n/
  -> Maybe [Ratio a] -- ^ The convergents of the continued fractions
                     --   representation of /sqrt(n)/.
sqrtConvergents n =
  sqrtContinuedFraction n <&> \(a0, as) ->
  [ continuedFractionConvergent (a0, as') | as' <- inits as ]
