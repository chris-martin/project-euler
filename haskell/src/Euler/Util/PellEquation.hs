-- | Pell's Equation: /x^2 - Dy^2 = 1/.

module Euler.Util.PellEquation
    ( fundamentalSolution
    , checkSolution
    ) where

import Euler.Prelude

import Euler.Util.ContinuedFractions (sqrtConvergents)

import qualified Data.List as List

---------------------------------------------------------

checkSolution :: (Integral a)
    => a      -- ^ /D/
    -> (a, a) -- ^ /(x, y)/
    -> Bool

fundamentalSolution :: (Integral a)
    => a      -- ^ /D/
    -> Maybe (a, a)
-- ^ The fundamental solution to Pell's Equation is the
-- solution /(x, y)/ minimizing /x/.

---------------------------------------------------------

checkSolution d (x, y) = (x^2) - (d * y^2) == 1

-- https://en.wikipedia.org/w/index.php?title=Pell%27s_equation&oldid=739131845#Fundamental_solution_via_continued_fractions

fundamentalSolution d = do
    c <- sqrtConvergents d
    s <- List.find (checkSolution d) $ (numerator &&& denominator) <$> c
    pure s
