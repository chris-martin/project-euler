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

import Data.Maybe (isJust)

-- $setup
-- >>> import Test.QuickCheck

---------------------------------------------------------

divides :: (Integral a, Integral b) => a -> b -> Bool
-- ^ @a ``divides`` b@ iff /a/ is a divisor of /b/;
-- in other words, /b/ = 0 (mod /a/)

square :: Num a => a -> a
-- ^ @'square' x@ = /x^2/

intSqrt :: Integral a => a -> Maybe a
-- ^ @'intSqrt' x@ is the integer /r/ such that /r^2 = x/, if
-- such an integer exists.
--
-- prop> \(NonNegative n) -> intSqrt (n^2) === Just n
--
-- prop> \(Positive n) -> intSqrt(n^2 + 1) === Nothing
--
-- prop> \(Positive n) -> intSqrt((n+1)^2 - 1) === Nothing

floorSqrt :: Integral a => a -> a
-- ^ @'floorSqrt' x@ is the largest integer /r/ such that
-- /r^2 <= x/.
--
-- prop> \(NonNegative n) -> floorSqrt (n^2) === n
--
-- prop> \(Positive n) -> floorSqrt(n^2 + 1) === n
--
-- prop> \(Positive n) -> floorSqrt((n+1)^2 - 1) === n

isSquare :: Integral a => a -> Bool
-- ^ @'isSquare' x@ iff @x@ is a square.
--
-- prop> \(NonNegative n) -> isSquare (square n)
--
-- prop> \(Positive n) -> not (isSquare $ (square n) + 1)

million :: Integral a => a
-- ^ One million = 1,000,000

---------------------------------------------------------

factorial :: Integral a => a -> Integer
-- ^ @'factorial' n@ (often written as /n!/) is the product
-- of the integers from 1 to /n/, inclusive.

factorials :: [Integer]
-- ^ All of the factorials, ascending. Equivalent to
-- @'map' 'factorial' [0..]@.
--
-- >>> take 10 factorials
-- [1,1,2,6,24,120,720,5040,40320,362880]

-- $factorialProperties
--
-- prop> \(NonNegative n) -> factorial n === factorials !! n

---------------------------------------------------------

d `divides` n = fromIntegral n `mod` fromIntegral d == 0

factorial n = product [1 .. fromIntegral n]

factorials = 1 : (scanl1 (*) [1..])

square x = x * x

million = 10 ^ 6

intSqrt n = searchWithGuess 0 n initialGuess where

    initialGuess = round $ sqrt $ fromIntegral n

    -- A rather naive binary search, between the inclusive bounds a and b.
    search a b
        | a > b     = Nothing
        | otherwise = let guess = a + ((b - a) `div` 2)
                      in  searchWithGuess a b guess

    searchWithGuess a b guess
        | sq == n = Just guess
        | sq >  n = search a (guess - 1)
        | sq <  n = search (guess + 1) b
        where sq = square guess

floorSqrt n = searchWithGuess 0 n initialGuess where

    initialGuess = floor $ sqrt $ fromIntegral n

    -- A rather naive binary search, between the inclusive bounds a and b.
    search a b
        | a > b      = undefined
        | a == b     = a
        | otherwise  = let guess = a + ((b - a) `div` 2)
                       in  searchWithGuess a b guess

    searchWithGuess a b guess
        | a == b                = a
        | sq1 == n              = guess
        | sq2 == n              = guess + 1
        | sq1 < n && sq2 > n    = guess
        | sq1 > n               = search a (guess - 1)
        | sq2 < n               = search (guess + 1) b
        where sq1 = square $ guess
              sq2 = square $ guess + 1

isSquare = isJust . intSqrt
