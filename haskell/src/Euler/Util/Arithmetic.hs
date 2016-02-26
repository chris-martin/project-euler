module Euler.Util.Arithmetic
    (
    -- * Functions
      divides
    , factorial, factorials
    , square
    , intSqrt

    -- * Constants
    , million
    ) where

---------------------------------------------------------

-- | @a ``divides`` b@ iff /a/ is a divisor of /b/;
-- in other words, /b/ = 0 (mod /a/)
divides :: (Integral a, Integral b) => a -> b -> Bool

-- | @'factorial' n@ (often written as /n!/) is the product
-- of the integers from 1 to /n/, inclusive. Equivalent to
-- @'factorials' '!!' n@.
factorial :: Integral a => a -> Integer

-- | All of the factorials, ascending. Equivalent to
-- @'map' 'factorial' [0..]@.
--
-- * /0! = 1/,
-- * /1! = 1/,
-- * /2! = 2/,
-- * /3! = 6/,
-- * /4! = 24/,
-- * ...
factorials :: [Integer]

-- | @'square' x@ = /x^2/
square :: Num a => a -> a

-- | @'intSqrt' x@ is the integer /r/ such that /r^2 = x/, if
-- such an integer exists.
intSqrt :: Integral a => a -> Maybe a

-- | One million = 1,000,000
million :: Integral a => a

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
        | otherwise = searchWithGuess a b $ a + ((b - a) `div` 2)

    searchWithGuess a b guess
        | sq == n = Just guess
        | sq >  n = search a (guess - 1)
        | sq <  n = search (guess + 1) b
        where sq = square guess
