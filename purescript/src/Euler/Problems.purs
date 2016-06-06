module Euler.Problems (answer) where

import Prelude (($), (==), pure, show, mod)

import Prim (Array, String)

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)

import Data.Array as Array
import Data.Foldable (any, sum)
import Data.Maybe (Maybe(..))

-- | @'answer' n@ calculates the answer to Euler problem /n/, or
-- evaluates to @Nothing@ if there is no solution known for problem /n/.
--
-- The answers to Project Euler problems are all numeric, but the type
-- here is 'String' rather than 'Integer' because in some cases the answer
-- is semantically a "list of digits" rather than an integer, which may
-- be an important distinction if the first digit of an answer is zero.
--
-- Most of the answers are pure, but some problems involve reading input
-- data from files.
answer :: forall e. Prim.Int -> Maybe (Aff (err :: EXCEPTION, fs :: FS | e) String)

answer 1 = Just $ pure $ show ans
  where ans = sum $ Array.filter f $ Array.range 1 999
        f n = any (\x -> x `divides` n) ([3, 5] :: Array Prim.Int)
        divides a b = b `mod` a == 0

answer _ = Nothing
