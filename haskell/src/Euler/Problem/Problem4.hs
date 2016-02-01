module Euler.Problem.Problem4 (answer) where

import Data.Sequence (replicateM)
import Data.Foldable (toList)

answer :: Integer
answer = maximum $ filter isPalindrome $ map product $ pairsOf [1..999]
  where
    isPalindrome i = s == reverse s where s = show i
    pairsOf xs = map toList $ replicateM 2 xs
