module Euler.Problems.Problem69
  ( answer
  ) where

import Euler.Prelude

import Euler.Util.Arithmetic (million)
import Euler.Util.List (maximumOn)
import Euler.Util.Prime (primes)

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.SetMap as SetMap

answer :: Int
answer =
    maximumOn (\n -> n % totient rc n) [2..bound]
  where
    bound = million
    rc = makeRC bound

newtype RC = RC { toSetMap :: SetMap Int Int }

rcToList :: RC -> [(Int, [Int])]
rcToList =
  fmap (fmap toList) . Map.toList . SetMap.toMap . toSetMap

totient :: RC -> Int -> Int
totient rc n =
  n - 1 - Foldable.length (SetMap.lookup n $ toSetMap rc)

{- |

>>> makeRC 10
[(4,[2]),(6,[2,3,4]),(8,[2,4,6]),(9,[3,6]),(10,[2,4,5,6,8])]

-}
makeRC :: Int -> RC
makeRC bound = RC $ foldr' (uncurry SetMap.insert) SetMap.empty $
    primes
    & List.takeWhile (<= bound)
    & (=<<) (\p ->
        List.iterate (\(m, xs) -> (m + p, m : xs)) (p, [])
        & List.takeWhile (\(m, _) -> m <= bound)
        & (=<<) (\(m, xs) -> fmap (\x -> (m, x)) xs)
      )

instance Show RC
  where
    show = brace . intercalate "," . fmap (show . fmap sort) . sort . rcToList
      where
        brace x = "[" <> x <> "]"
