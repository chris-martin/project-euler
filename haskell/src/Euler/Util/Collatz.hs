-- | Collatz sequences are introduced in Euler problem 14.

module Euler.Util.Collatz
    ( collatz
    , collatzLengths
    , Lengths
    ) where

import Euler.Prelude

import qualified Data.Map as Map

--------------------------------------------------------------------------------

{- |

One step of the Collatz sequence, defined for the set of positive integers:

* /n → n\/2/ (/n/ is even)
* /n → 3n + 1/ (/n/ is odd)

The example from problem 14:

>>> takeWhile (/= 1) $ iterate collatz 13
[13,40,20,10,5,16,8,4,2]

-}
collatz :: Natural -> Natural
collatz i = if even i then i `div` 2 else 3 * i + 1

{- |

A mapping from /n/ to the length of the Collatz sequence starting
from /n/.

-}
type Lengths = Map Natural Natural

{- |

@'collatzLengths' xs@ evaluates to a 'Lengths' mapping which contains the
collatz length for at least every element of @xs@ (and possibly more).

The example from problem 14:

>>> collatzLengths [13] Map.! 13
10

-}
collatzLengths :: [Natural] -> Lengths
collatzLengths = getLengths (Map.singleton 1 1)

--------------------------------------------------------------------------------

getLengths :: Lengths -> [Natural] -> Lengths

-- The stack is empty, we're done.
getLengths lengths [] = lengths

-- The next item x on the stack is already calculated, so just pop it off.
getLengths lengths (x:restOfStack) | Map.member x lengths =
    getLengths lengths restOfStack

-- We're looking at the tree edge x -> y, where length(x) is unknown.
getLengths lengths stack@(x:restOfStack) =
    let y = collatz x
    in case Map.lookup y lengths of

      -- length(y) is already known.
      Just yLength -> let newLengths = Map.insert x (yLength + 1) lengths
                      in getLengths newLengths restOfStack

      -- We haven't learned anything except that there's a new
      -- value y that we need to know; push y onto the stack.
      Nothing      -> getLengths lengths (y:stack)
