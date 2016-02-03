{-# LANGUAGE NamedFieldPuns #-}

module Euler.Util.Prime.FactorEnum
    ( Fint(..)
    , Prime
    , Pow
    , InfPows
    , carry
    , double
    , one
    , two
    , factorizations
    , primesToFint
    , carryUntilLTE
    , intToFint
    , allFints
    ) where

import Data.Foldable       ( toList )
import Data.Function       ( on )
import Data.Maybe          ( catMaybes, isJust, fromJust )
import Data.Numbers.Primes ( primes )
import Data.Sequence       ( Seq, (<|), (|>) )

import Euler.Util.Prime.Factoring ( primeFactors )

import qualified Data.Sequence as Seq
import qualified Data.MultiSet as MultiSet

type Prime = Integer

-- | Prime power @(p, e)@ represents @p ^ e@, where p is prime.
type Pow = (Prime, Integer)

-- | The elements of this tuple form a list of @(p, e)@, where the
-- @p@ are consecutive primes, such that @Π p^e = 'value'@.
--
-- Because the list ends in an infinite series of @'Pow's@ with zero
-- exponents, it is split into a 2-tuple where the 'snd' element
-- contains all of the trailing zeros.
type InfPows = (Seq Pow, [Pow])

-- | A positive integer represented as its prime factorization.
--
-- Mnemonic: "factorized int".
--
-- Examples:
--
-- > Fint { value = 50
-- >      , isPrimePower = False
-- >      , digits = [ (2, 1), (3, 0), (5, 2) ]
-- >      , greaterPrimes = [ 7, 11, 13, ... ]
-- >      }
--
-- > Fint { value = 125
-- >      , isPrimePower = True
-- >      , digits = [ (2, 0), (3, 0), (5, 3) ]
-- >      , greaterPrimes = [ 7, 11, 13, ... ]
-- >      }
data Fint = Fint
    { fintValue :: Integer
    -- ^ The number

    , fintPrimePowerBase :: Maybe Prime
    -- ^ Whether the number is a prime power (in which case its 'digits'
    -- has only one value with a nonzero exponent)

    , fintPows :: InfPows
    -- ^ The exponent for every prime
    }

instance Eq Fint where
    -- Equality could just be based on 'fintValue', but we get better
    -- coverage from tests that compare @'Fint's@ by checking all of
    -- the attributes.
    (==) = (==) `on` (\x -> ( fintValue x
                            , fintPrimePowerBase x
                            , fst $ fintPows x
                            , head $ snd $ fintPows x
                            ))

instance Ord Fint where compare = compare `on` fintValue

instance Show Fint where
    show f = let value  = show $ fintValue             f
                 ppBase = show $ fintPrimePowerBase    f
                 pows1  = show $        fst $ fintPows f
                 pows2  = show $ head $ snd $ fintPows f
             in "Fint { value = " ++ value ++ ", primePowerBase = "
                ++ ppBase ++ ", fintPows = (" ++ pows1
                ++ ", [" ++ pows2 ++ ", ...]) }"

-- | Example: @'powsToPrimes' [(2, 1), (3, 0), (5, 2)] = [2, 5, 5]@
powsToPrimes :: Foldable f => f Pow -> [Prime]
powsToPrimes = concatMap $ \(p, i) -> replicate (fromIntegral i) p

-- | The integer value represented by a factorization, @Π p^e@
--
-- Example: @'powsValue' [(2, 1), (3, 0), (5, 2)] = 50@
powsToValue :: Foldable f => f Pow -> Integer
powsToValue = product . powsToPrimes

-- | Example: @'fintPrimes' ('Fint' { value = 50, ... }) = [2, 5, 5]@
fintToPrimes :: Fint -> [Prime]
fintToPrimes = powsToPrimes . toList . fst . fintPows

-- | Example: @'primesToPows' [2, 5, 5] = [(2, 1), (5, 2)]@
primesToPows :: [Prime] -> [Pow]
primesToPows = (map (\(p, i) -> (p, fromIntegral i))) .
               MultiSet.toAscOccurList . MultiSet.fromList

-- | Example:
--
-- > 'primesToInfPows' [(2, 1), (5, 2)] = ( [(2, 1), ( 3, 0), (5, 2)]
-- >                                      , [(7, 0), (11, 0), ...   ] )
powsToInfPows :: [Pow] -> InfPows
powsToInfPows pows = f pows primes where

    f :: [Pow] -> [Prime] -> InfPows
    f [] primes = (Seq.empty, map (\p -> (p, 0)) primes)
    f ((p1, i):pows) (p2:primes) | p1 == p2 =
        infPowsCons (p1, i) (f pows primes)
    f pows (p:primes) = infPowsCons (p, 0) (f pows primes)

    infPowsCons :: Pow -> InfPows -> InfPows
    infPowsCons x (xs, ys) = (x <| xs, ys)

primesToInfPows :: [Prime] -> InfPows
primesToInfPows = powsToInfPows . primesToPows

powsToFint :: [Pow] -> Fint
powsToFint pows = Fint
    { fintValue          = powsToValue pows
    , fintPrimePowerBase = case pows of
                             [(p, i)] -> Just p
                             _        -> Nothing
    , fintPows           = powsToInfPows pows
    }

primesToFint :: [Prime] -> Fint
primesToFint = powsToFint . primesToPows

intToFint :: Integer -> Fint
intToFint = primesToFint . primeFactors

one :: Fint
one = powsToFint []

two :: Fint
two = powsToFint [(2, 1)]

-- | Multiplication by two.
double :: Fint -> Fint
double Fint { fintValue = 1 } = two
double (Fint { fintValue, fintPows = (xs, ys) }) =
    Fint { fintValue          = fintValue * 2
         -- The result is a prime power iff the input is a power of two
         , fintPrimePowerBase = if length xs == 1 then Just 2 else Nothing
         , fintPows           = (Seq.adjust (fmap (+ 1)) 0 xs, ys)
         }

-- | Drop the least nonzero cardinality to zero and increment the next
-- greatest cardinality, similar to performing a carry when adding one
-- with a digit representation of an integer.
carry :: Fint -> Fint
carry Fint { fintPrimePowerBase = Nothing, fintPows = (xs, ys) } =
    let q   = fromJust $ Seq.findIndexL ((/= 0) . snd) xs
        xs' = Seq.adjust (fmap (\_ -> 0))  q      $
              Seq.adjust (fmap (+ 1))     (q + 1) $ xs
    in  Fint { fintValue          = powsToValue xs'
             , fintPrimePowerBase = if q == length xs - 2
                                    then Just $ fst $ xs `Seq.index` (q+1)
                                    else Nothing
             , fintPows           = (xs', ys)
             }
carry Fint { fintPrimePowerBase = Just _, fintPows = (xs, (y, 0):ys) } =
    let xs' = Seq.adjust (fmap (\_ -> 0)) (length xs - 1) xs |> (y, 1)
    in  Fint { fintValue          = powsToValue xs'
             , fintPrimePowerBase = Just y
             , fintPows           = (xs', ys)
             }

-- | Repeatedly apply 'carry' until @f <= n@, or else
-- evaluate to 'Nothing' if the series diverges.
carryUntilLTE :: Integer -> Fint -> Maybe Fint
carryUntilLTE n f
    | (fintValue f) <= n                       = Just f
    | maybe False (> n) (fintPrimePowerBase f) = Nothing
    | otherwise                                = carryUntilLTE n $ carry f

nextFint :: Integer -> Fint -> Maybe Fint
nextFint max f = carryUntilLTE max $ double f

-- | Prime factorizations of numbers in @[1 .. n]@ in no particular order.
allFints :: Integer -> [Fint]
allFints n = catMaybes $ takeWhile isJust $ iterate (>>= (nextFint n)) $ Just one

factorizations :: Integer -> [(Integer, [Prime])]
factorizations = (map (\f -> (fintValue f, fintToPrimes f))) . allFints
