-- | The Collatz sequence is defined for the set of positive integers:
--
-- * /n → n\/2/ (/n/ is even)
-- * /n → 3n + 1/ (/n/ is odd)

module Euler.Util.Collatz (collatz, collatzLengths) where

import Data.Map (Map)
import qualified Data.Map as Map

collatz :: Integer -> Integer
collatz i = if even i then i `div` 2 else 3 * i + 1

type Lengths = Map Integer Integer

collatzLengths :: [Integer] -> Lengths
collatzLengths = getLengths $ Map.singleton 1 1

getLengths :: Lengths -> [Integer] -> Lengths
-- The stack is empty, we're done!
getLengths lengths [] = lengths
getLengths lengths stack@(x:restOfStack)
  -- The next item x on the stack is already calculated, so just pop it off.
  | Map.member x lengths = getLengths lengths restOfStack

  -- We're looking at the tree edge x -> y, where length(x) is unknown.
  | otherwise =
      let y = collatz x
      in case Map.lookup y lengths of

          -- length(y) is already known.
          Just yLength -> let newLengths = Map.insert x (yLength + 1) lengths
                          in getLengths newLengths restOfStack

          -- We haven't learned anything except that there's a new
          -- value y that we need to know; push y onto the stack.
          Nothing      -> getLengths lengths (y:stack)
