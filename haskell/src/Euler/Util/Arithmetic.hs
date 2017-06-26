{-# LANGUAGE ScopedTypeVariables #-}

module Euler.Util.Arithmetic
  (
  -- $setup

  -- * Functions
    divides
  , square
  , intSqrt
  , floorSqrt
  , isSquare

  -- * Factorial
  -- ** Functions
  , factorial, factorials
  -- ** Properties
  -- $factorialProperties

  -- * Constants
  , million
  ) where

import Euler.Prelude

import qualified Data.Foldable as Foldable
import qualified Data.List as List

-- $setup
-- >>> import Test.QuickCheck

---------------------------------------------------------

{- |

@a ``divides`` b@ iff /a/ is a divisor of /b/; in other words, /b/ = 0 (mod
/a/).

-}
divides :: Integral a => a -> a -> Bool
d `divides` n = n `mod` d == 0

{- |

@'square' x@ = /x^2/

-}
square :: Num a => a -> a
square x = x * x

{- |

@'intSqrt' x@ is the integer /r/ such that /r^2 = x/, if such an integer exists.

prop> \(NonNegative n) -> intSqrt (n^2) === Just n

prop> \(Positive n) -> intSqrt(n^2 + 1) === Nothing

prop> \(Positive n) -> intSqrt((n+1)^2 - 1) === Nothing

-}
intSqrt :: Integral a => a -> Maybe a
intSqrt n = searchWithGuess 0 n initialGuess where

  initialGuess = round (sqrt (fromIntegral n) :: Double)

  -- A rather naive binary search, between the inclusive bounds a and b.
  search a b =
    if a > b
      then Nothing
      else let guess = a + ((b - a) `div` 2)
           in  searchWithGuess a b guess

  searchWithGuess a b guess =
    case compare (square guess) n of
      EQ -> Just guess
      GT -> search a (guess - 1)
      LT -> search (guess + 1) b

{- |

@'floorSqrt' x@ is the largest integer /r/ such that /r^2 <= x/.

prop> \(NonNegative n) -> floorSqrt (n^2) === n

prop> \(Positive n) -> floorSqrt(n^2 + 1) === n

prop> \(Positive n) -> floorSqrt((n+1)^2 - 1) === n

-}
floorSqrt :: Integral a => a -> a
floorSqrt n = searchWithGuess 0 n initialGuess where

  initialGuess = floor (sqrt (fromIntegral n) :: Double)

  -- A rather naive binary search, between the inclusive bounds a and b.
  search a b =
    case compare a b of
      GT -> undefined
      EQ -> a
      LT -> let guess = a + ((b - a) `div` 2)
            in  searchWithGuess a b guess

  searchWithGuess a b guess
      | a == b                = a
      | sq1 == n              = guess
      | sq2 == n              = guess + 1
      | sq1 < n && sq2 > n    = guess
      | sq1 > n               = search a (guess - 1)
      | sq2 < n               = search (guess + 1) b
      where sq1 = square guess
            sq2 = square (guess + 1)

{- |

@'isSquare' x@ iff @x@ is a square.

prop> \(NonNegative n) -> isSquare (square n)

prop> \(Positive n) -> not (isSquare $ (square n) + 1)

-}
isSquare :: Integral a => a -> Bool
isSquare = isJust . intSqrt

{- |

One million = /1,000,000/.

-}
million :: Integral a => a
million = 10 ^ (6 :: Int)

--------------------------------------------------------------------------------

{- |

@'factorial' n@ (often written as /n!/) is the product of the integers from 1 to
/n/, inclusive.

-}
factorial :: Integral a => a -> Integer
factorial n = Foldable.product [1 .. fromIntegral n]

{- |

All of the factorials, ascending. Equivalent to @'map' 'factorial' [0..]@.

>>> take 10 factorials
[1,1,2,6,24,120,720,5040,40320,362880]

-}
factorials :: [Integer]
factorials = 1 : (List.scanl1 (*) [1..])

{-

$factorialProperties

prop> \(NonNegative n) -> factorial n === factorials !! n

-}
