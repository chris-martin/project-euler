module Problem12 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Numbers.Primes (primes)

answer :: Integer
answer = head $ filter hasEnoughFactors $ triangles where
    hasEnoughFactors n = numberOfFactors n > 500

triangles :: [Integer]
triangles = scanl1 (+) [1..]

numberOfFactors :: Integer -> Integer
numberOfFactors n = product $ map (+1) $ Map.elems $ factor n

-- Maps prime factors to cardinalities
factor :: Integer -> Map Integer Integer
factor n = factor' Map.empty n where
    factor' f' 1 = f'
    factor' f' n' = factor' f'' n'' where
        f'' = Map.alter ((Just).(+1).(fromMaybe 0)) d f'
        n'' = (n' `div` d)
        d = lowestPrimeFactor n'

lowestPrimeFactor :: Integer -> Integer
lowestPrimeFactor n = head $ filter (`divides` n) primes where
    n' `divides` d = d `mod` n' == 0
