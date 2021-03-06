module Euler.Util.Polynomial
  ( Coefficients
  , eval
  , hasIntRootBetween
  , findIntRootBetween
  ) where

import Euler.Prelude

-------------------------------------------------------------------------------

{- |

Polynomials are represented by coefficient lists. For example, /7 + 5x - 4x^2 +
12x^4/ has coefficients @[7, 5, -4, 0, 12]@.

-}
type Coefficients = []

{- |

@'eval' cfs x@ is the value of the polynomial with coefficients @cfs@ at point
@x@

-}
eval :: (Num a) => Coefficients a -> a -> a
eval cfs x =
  -- Horner's rule for polynomial evaluation
  foldr' (\a b -> a + b * x) 0 cfs

{- |

@'findIntRootBetween' cfs a b@ is an integral root of the polynomial having
coefficients @cfs@ on the inclusive interval /[a, b]/, if one exists.

-}
findIntRootBetween :: (Integral a) => Coefficients a -> a -> a -> Maybe a
findIntRootBetween cfs x1 x2 =
  findIntRootBetweenP cfs (evalP cfs x1) (evalP cfs x2)

{- |

@'hasIntRootBetween' cfs a b@ tells whether there exists an integral root of the
polynomial having coefficients @cfs@ on the inclusive interval /[a, b]/.

-}
hasIntRootBetween :: (Integral a) => Coefficients a -> a -> a -> Bool
hasIntRootBetween cfs a b =
  isJust (findIntRootBetween cfs a b)

-------------------------------------------------------------------------------

data P a = P { px :: a, py :: a }

evalP :: (Num a) => [a] -> a -> P a
evalP cfs x = P { px = x, py = eval cfs x }

findIntRootBetweenP :: (Integral a) => [a] -> P a -> P a -> Maybe a
findIntRootBetweenP cfs p1@P{ px = x1, py = y1 }
                        p2@P{ px = x2, py = y2 }
    | y1 == 0                = Just x1
    | y2 == 0                = Just x2
    | signum y1 == signum y2 = Nothing
    | abs (x1 - x2) <= 1     = Nothing
    | otherwise              = recur
  where
    recur = findIntRootBetweenP cfs mid $
        if signum (py mid) == signum y1 then p2 else p1
    mid = evalP cfs $ min x1 x2 + abs (x1 - x2) `div` 2
