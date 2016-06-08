module Euler.Problems (answer) where

import Euler.Util.Fibonacci (fibs)

import Prelude (($), (==), (<), (<<<), (*), pure, show, mod)

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)

import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Foldable (any, sum)
import Data.Int (even)
import Data.Int as Int
import Data.List (takeWhile)
import Data.List as List
import Data.List.Lazy as ZList
import Data.Maybe (Maybe(..))

import Math (pow)

millionBigInt :: BigInt
millionBigInt = (BigInt.fromInt 4) * ((BigInt.fromInt 10) `BigInt.pow` (BigInt.fromInt 6))

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
answer :: forall e. Int -> Maybe (Aff (err :: EXCEPTION, fs :: FS | e) String)

answer 1 = Just $ pure $ show ans
  where ans = sum $ Array.filter f $ Array.range 1 999
        f n = any (\x -> x `divides` n) ([3, 5] :: Array Int)
        divides a b = b `mod` a == 0

answer 2 = Just $ pure $ BigInt.toString ans
  where ans = (sum <<< (ZList.filter BigInt.even) <<< (ZList.takeWhile (\x -> x < millionBigInt))) fibs

answer _ = Nothing
