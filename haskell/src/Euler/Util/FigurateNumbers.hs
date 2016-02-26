-- | <https://en.wikipedia.org/wiki/Figurate_number Figurate numbers>
-- are sets of numbers related to polygon shapes.
module Euler.Util.FigurateNumbers
    ( pentagonals
    , pentagonalN
    , isPentagonal
    ) where

import qualified Euler.Util.Polynomial as Poly

-- | All of the pentagonals /P_1, P_2, .../
pentagonals :: Integral a => [a]
pentagonals = map snd $
    iterate (\(n, p) -> (n + 1,
                         p + 3 * (n + 1) - 2))
            (1, 1)

-- | @'pentagonalN' n@ = /P_n/
pentagonalN :: Integral a => a -> a
pentagonalN n = n * (3 * n - 1) `div` 2

-- | @'isPentagonal' p@ determines whether 'pentagonals' contains @p@,
-- determined by testing whether the solution of /0 = (3n^2 - n - 2p)/
-- is integral.
isPentagonal :: Integral a => a -> Bool
isPentagonal p = Poly.hasIntRootBetween [-2 * p, -1, 3] 0 p
