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

divides :: (Integral a, Integral b) => a -> b -> Bool

factorial :: Integral a => a -> Integer
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

    -- a and b are inclusive bounds
    search a b
        | a > b     = Nothing
        | otherwise = searchWithGuess a b $ a + ((b - a) `div` 2)

    searchWithGuess a b guess
        | sq == n = Just guess
        | sq >  n = search a (guess - 1)
        | sq <  n = search (guess + 1) b
        where sq = square guess
