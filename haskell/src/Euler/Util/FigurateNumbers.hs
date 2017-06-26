{- |

<https://en.wikipedia.org/wiki/Figurate_number Figurate numbers> are sets of
numbers related to polygon shapes.

-}
module Euler.Util.FigurateNumbers
  (
  -- $setup

  -- * Triangle
    triangles
  , triangleN
  , isTriangle
  -- ** Properties
  -- $triangleProperties

  -- * Pentagonal
  , pentagonals
  , pentagonalN
  , isPentagonal
  -- ** Properties
  -- $pentagonalProperties

  -- * Hexagonal
  , hexagonals
  , hexagonalN
  , isHexagonal
  -- ** Properties
  -- $hexagonalProperties

  ) where

import Euler.Prelude

import qualified Euler.Util.Polynomial as Poly

import qualified Data.List as List

{- $setup

>>> import Test.QuickCheck
>>> import Data.List.Ordered (member, isSorted)

-}

--------------------------------------------------------------------------------
--  Triangle
--------------------------------------------------------------------------------

{- | All of the triangle numbers, ascending.

>>> take 5 triangles
[1,3,6,10,15]

-}
triangles :: Integral a => [a]

{- |

@'triangleN' n@ is the /n/th triangle number.

-}
triangleN :: Integral a => a -> a

{- |

@'isTriangle' x@ denotes whether 'triangles' contains @x@.

-}
isTriangle :: Integral a => a -> Bool

{- $triangleProperties

prop> \(Positive n) -> isTriangle n == member n triangles

prop> isSorted (take 100 triangles)

-}

--------------------------------------------------------------------------------
--  Pentagonal
--------------------------------------------------------------------------------

{- |

All of the pentagonal numbers, ascending.

>>> take 10 pentagonals
[1,5,12,22,35,51,70,92,117,145]

-}
pentagonals :: Integral a => [a]

{- |

@'pentagonalN' n@ is the /n/th pentagonal number.

-}
pentagonalN :: Integral a => a -> a

{- |

@'isPentagonal' x@ denotes whether 'pentagonals' contains @x@.

-}
isPentagonal :: Integral a => a -> Bool

{- $pentagonalProperties

prop> \(Positive n) -> isPentagonal n == member n pentagonals

prop> isSorted (take 100 pentagonals)

-}

--------------------------------------------------------------------------------
--  Hexagonal
--------------------------------------------------------------------------------

{- |

All of the hexagonal numbers, ascending.

>>> take 5 hexagonals
[1,6,15,28,45]

-}
hexagonals :: Integral a => [a]

{- |

@'hexagonalN' n@ is the /n/th hexagonal number.

-}
hexagonalN :: Integral a => a -> a

{- |

@'isHexagonal' x@ denotes whether 'hexagonals' contains @x@.

-}
isHexagonal :: Integral a => a -> Bool

{- $hexagonalProperties

prop> \(Positive n) -> isHexagonal n == member n hexagonals

prop> isSorted (take 100 hexagonals)

-}

--------------------------------------------------------------------------------
--  Lookup by index
--------------------------------------------------------------------------------

triangleN   n = ( n * (    n + 1) ) `div` 2
pentagonalN n = ( n * (3 * n - 1) ) `div` 2
hexagonalN  n =   n * (2 * n - 1)


--------------------------------------------------------------------------------
--  Iteration
--------------------------------------------------------------------------------

triangles = fmap triangleN [1..]

pentagonals =
  fmap snd $
  List.iterate (\(n, p) -> (n + 1,
                            p + 3 * (n + 1) - 2))
               (1, 1)

hexagonals = fmap hexagonalN [1..]


--------------------------------------------------------------------------------
--  Testing
--------------------------------------------------------------------------------

{-

Tests are done by checking whether the solution of some polynomial is integral.

-}

-- 0 = -2x + n + n^2
isTriangle   x = Poly.hasIntRootBetween [ -2 * x,  1, 1 ] 0 x

-- 0 = 3n^2 - n - 2x
isPentagonal x = Poly.hasIntRootBetween [ -2 * x, -1, 3 ] 0 x

-- 0 = -2x -n + 2n^2
isHexagonal  x = Poly.hasIntRootBetween [     -x, -1, 2 ] 0 x
