-- | <https://en.wikipedia.org/wiki/Figurate_number Figurate numbers>
-- are sets of numbers related to polygon shapes.
module Euler.Util.FigurateNumbers
    (
    -- * Triangle
      triangles
    , triangleN
    , isTriangle

    -- * Pentagonal
    , pentagonals
    , pentagonalN
    , isPentagonal

    -- * Hexagonal
    , hexagonals
    , hexagonalN
    , isHexagonal

    ) where

import qualified Euler.Util.Polynomial as Poly

-----------------------------------------------------------
--  Triangle
-----------------------------------------------------------

-- | All of the triangle numbers, ascending
triangles :: Integral a => [a]
triangles = map triangleN [1..]

-- | @'triangleN' n@ is the /n/th triangle number
triangleN :: Integral a => a -> a
triangleN n = (n * (n + 1)) `div` 2

-- | @'isTriangle' x@ determines whether 'triangles' contains @x@,
-- determined by testing whether the solution of /0 = -2x + n + n^2/
-- is integral.
isTriangle :: Integral a => a -> Bool
isTriangle x = Poly.hasIntRootBetween [-2 * x, 1, 1] 0 x


-----------------------------------------------------------
--  Pentagonal
-----------------------------------------------------------

-- | All of the pentagonal numbers, ascending
pentagonals :: Integral a => [a]
pentagonals = map snd $
    iterate (\(n, p) -> (n + 1,
                         p + 3 * (n + 1) - 2))
            (1, 1)

-- | @'pentagonalN' n@ is the /n/th pentagonal number
pentagonalN :: Integral a => a -> a
pentagonalN n = (n * (3 * n - 1)) `div` 2

-- | @'isPentagonal' x@ determines whether 'pentagonals' contains @x@,
-- determined by testing whether the solution of /0 = 3n^2 - n - 2x/
-- is integral.
isPentagonal :: Integral a => a -> Bool
isPentagonal x = Poly.hasIntRootBetween [-2 * x, -1, 3] 0 x


-----------------------------------------------------------
--  Hexagonal
-----------------------------------------------------------

-- | All of the hexagonal numbers, ascending
hexagonals :: Integral a => [a]
hexagonals = map hexagonalN [1..]

-- | @'hexagonalN' n@ is the /n/th hexagonal number
hexagonalN :: Integral a => a -> a
hexagonalN n = n * (2 * n - 1)

-- | @'isHexagonal' x@ determines whether 'hexagonals' contains @x@,
-- determined by testing whether the solution of /0 = -2x -n + 2n^2/
-- is integral.
isHexagonal :: Integral a => a -> Bool
isHexagonal x = Poly.hasIntRootBetween [-x, -1, 2] 0 x
