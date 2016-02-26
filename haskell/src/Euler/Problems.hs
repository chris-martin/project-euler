{-# LANGUAGE OverloadedStrings #-}

module Euler.Problems (answer) where

import Euler.Util.Arithmetic ( divides, factorial, intSqrt, million, square )
import Euler.Util.Amicable   ( amicableNumbers )
import Euler.Util.Collatz    ( collatzLengths )
import Euler.Util.Date       ( monthLength )
import Euler.Util.Decimal    ( repetendLength )
import Euler.Util.Digit      ( intPalindrome, textDigits, textIntMaybe )
import Euler.Util.Fibonacci  ( fibs )
import Euler.Util.List       ( countDistinct, maximumOn, mode, sliding )
import Euler.Util.Map        ( keyWithMaxValue )
import Euler.Util.Pandigital ( pandigitals, pandigitalsRev )
import Euler.Util.Prime      ( countDivisors, largestPrimeFactor )

import Euler.Util.FigurateNumbers

import qualified Euler.Util.NumberWords  as NumberWords
import qualified Euler.Util.TrianglePath as TrianglePath

import qualified Euler.Problems.Problem9  as Problem9
import qualified Euler.Problems.Problem11 as Problem11
import qualified Euler.Problems.Problem15 as Problem15
import qualified Euler.Problems.Problem22 as Problem22
import qualified Euler.Problems.Problem23 as Problem23
import qualified Euler.Problems.Problem31 as Problem31
import qualified Euler.Problems.Problem33 as Problem33
import qualified Euler.Problems.Problem42 as Problem42
import qualified Euler.Problems.Problem43 as Problem43

import Prelude ( (==), (/=), (<=), (<), (>=), (>), (++), (!!), (.), ($)
               , (*), (+), (-), (&&), (^), (/)
               , all, and, any, concatMap, cycle, elem, even, filter
               , fromIntegral, last, length, head, map
               , maximum, not, null, product, pure, return, scanl1
               , splitAt, sum, toInteger, uncurry
               , zip, zipWith
               , Bool(..), Int, Integer, Integral, Show(..), String, IO )

import Control.Monad         ( guard )
import Data.Digits           ( digits, unDigits )
import Data.Foldable         ( toList )
import Data.List             ( dropWhile, findIndex, permutations, sort
                             , take, takeWhile )
import Data.Maybe            ( catMaybes, fromJust )
import Data.Numbers.Primes   ( isPrime, primes )
import Data.Sequence         ( replicateM )
import Data.Text             ( Text )

import qualified Data.List       as List
import qualified Data.Set        as Set
import qualified Data.Text       as Text
import qualified Data.Text.IO    as TextIO

-------------------------------------------------------------------------

inputText :: Int -> IO Text
inputText i = TextIO.readFile ("../problems/" ++ (show i) ++ "-data.txt")

showInteger :: Integer -> String
showInteger = show

showInt :: Int -> String
showInt = show

-------------------------------------------------------------------------

-- | @'answer' n@ calculates the answer to Euler problem /n/, or
-- evaluates to @"?"@ if there is no solution known for problem /n/.
--
-- The answers to Project Euler problems are all numeric, but the type
-- here is 'String' rather than 'Integer' because in some cases the answer
-- is semantically a "list of digits" rather than an integer, which may
-- be an important distinction if the first digit of an answer is zero.
--
-- Most of the answers are pure, but 'IO' is allowed because some problems
-- involve reading input data from files.
answer :: Integral a => a -> IO String

answer 1 = pure (showInteger a)
  where a = sum (filter f [1..999])
        f n = any (`divides` n) ([3, 5] :: [Integer])

answer 2 = pure (showInteger a)
  where a = (sum . (filter even) . (takeWhile (< 4 * million))) fibs

answer 3 = pure (showInteger a)
  where a = largestPrimeFactor (600851475143 :: Integer)

answer 4 = pure (showInteger a)
  where a = (maximum . (filter (intPalindrome 10)) . (map product))
            (pairsOf [1..999])
        pairsOf xs = map toList (replicateM 2 xs)

answer 5 = pure (showInteger a)
  where a = product factors
        -- greatest powers of primes within the bound
        factors :: [Integer]
        factors = map powerUp (takeWhile (<= bound) primes)

        powerUp n = last (takeWhile (<= bound) (powersOf n))
        powersOf n = map (n^) [(1 :: Int) ..]
        bound = 20

answer 6 = pure (showInteger a)
  where a = (square (sum xs)) - (sum (map square xs))
        xs = [1..100]

answer 7 = pure (showInteger a)
  where a = primes !! 10000

answer 8 = do text <- inputText 8
              return (f text)
  where f = showInteger . maximum . (map product) . (sliding 5) .
            (map toInteger) . textDigits

answer 9 = pure (showInteger Problem9.answer)

answer 10 = pure (showInteger a)
  where a = sum (takeWhile (< 2 * million) primes)

answer 11 = do text <- inputText 11
               return (show (Problem11.answer text))

answer 12 = pure (showInteger a)
  where a = (fromJust . (List.find f)) (scanl1 (+) [1..])
        f n = countDivisors n > (500 :: Integer)

answer 13 = do text <- inputText 13
               return (f text)
  where
    f = (take 10) . showInteger . sum . parseNumbers
    parseNumbers = catMaybes . map textIntMaybe . Text.lines

answer 14 = pure (showInteger a)
  where a = (keyWithMaxValue . collatzLengths) [1 .. million]

answer 15 = pure (showInteger Problem15.answer)

answer 16 = pure (showInteger a)
  where a = sum (digits 10 (2 ^ (1000 :: Integer)))

answer 17 = pure (showInt a)
  where a = sum (map (length . NumberWords.word) [1..1000])

answer 18 = do text <- inputText 18
               return (show (f text))
  where f = TrianglePath.reduceTriangle . TrianglePath.parseTriangle

answer 19 = pure (showInt a)
  where a = length (filter isMatchingDate datesWithWeekday)
        dates = do year  <- [1900 .. 2000] :: [Int]
                   month <- [1 .. 12]      :: [Int]
                   day   <- [1 .. monthLength year month]
                   return (year, month, day)

        datesWithWeekday = zip dates (cycle ([1 .. 7] :: [Int]))

        isMatchingDate ((year, _month, day), weekday) =
            year /= 1900 && day == 1 && weekday == 7

answer 20 = pure (showInteger a)
  where a = (sum . (digits 10) . factorial) (100 :: Integer)

answer 21 = pure (showInteger a)
  where a = sum (amicableNumbers 9999)

answer 22 = do text <- inputText 22
               return (showInteger (Problem22.answer text))

answer 23 = pure (showInteger Problem23.answer)

answer 24 = pure ((sort (permutations ['0'..'9'])) !! (million - 1))

answer 25 = pure (showInt a)
  where a = (fromJust . (findIndex (>= x))) (fibs :: [Integer])
        x = 10 ^ (999 :: Integer)

answer 26 = pure (showInteger a)
  where a = maximumOn f [1..999]
        f = repetendLength . (1 /) . fromIntegral

answer 27 = pure (showInt ans)
  where ans = ((uncurry (*)) . (maximumOn nrOfPrimes)) expressions
        expressions = do a <- range; b <- range; return (a, b)
                      where x = 999; range = [-x..x]
        apply (a, b) n = n*n + a*n + b
        nrOfPrimes e = (length . (takeWhile (isPrime . (apply e)))) [0..]

answer 28 = pure (showInteger a)
  where
    a = 1 + sum (concatMap f [1..500])
    f i = let j = 2 * i
              x = square (j + 1)
          in  [x, x - j, x - 2*j, x - 3*j]

answer 29 = pure (showInt a)
  where a = countDistinct [a ^ b | a <- r, b <- r]
        r = [2..100] :: [Integer]

answer 30 = pure (showInteger a)
  where a = sum (filter isMagic [(2 :: Integer) .. (maxPowerSum maxNrOfDigits)])
        maxPowerSum = (* (pow5 9))
        minValue n = 10 ^ (n - 1)
        isFeasible n = maxPowerSum n >= minValue n
        maxNrOfDigits = (last . (takeWhile isFeasible)) [1..]
        isMagic n = ((== n) . sum . (map pow5) . (digits 10)) n
        pow5 = (^ (5 :: Integer))

answer 31 = pure (showInteger Problem31.answer)

answer 32 = pure (showInteger a)
  where a = (sum . Set.fromList) zs
        zs = do p <- permutations [1..9]
                let (q, zs) = splitAt 5 p
                    z = unDigits 10 zs
                xl <- [1, 2]
                let (xs, ys) = splitAt xl q
                    x = unDigits 10 xs
                    y = unDigits 10 ys
                guard (x * y == z)
                return z

answer 33 = pure (showInteger Problem33.answer)

answer 34 = pure (showInteger a)
  where a = sum (filter isCurious [3 .. 10 ^ maxDigits])
        maxDigits = last (takeWhile f [1..])
          where f i = (factorial (9 :: Integer)) * i >= 10 ^ i
        isCurious :: Integer -> Bool
        isCurious n = ((== n) . sum . (map factorial) . (digits 10)) n

answer 35 = pure (showInt a)
  where a = (length . (filter ((all isPrime) . digitRotations)))
            ([2 .. million - 1] :: [Integer])
        digitRotations x = ((map (unDigits 10)) . listRotations . (digits 10)) x
        listRotations xs = ((map (take l)) . (take l) . List.tails . cycle) xs
          where l = length xs

answer 36 = pure (showInteger a)
  where a = sum (filter f [1 .. million - 1])
        f x = intPalindrome 2 x && intPalindrome 10 x

answer 37 = pure (showInteger a)
  where a = (sum . (take 11) . (filter ((all isPrime) . digitTruncations))) [11..]
        digitTruncations = (map (unDigits 10)) . truncations . (digits 10)
        truncations xs = filter (not . null) (List.tails xs ++ List.inits xs)

answer 38 = pure (showInteger a)
  where a =  maximum (filter pan9 xs)
        xs = concatMap (\k -> takeWhile (< 10^(9 :: Integer))
             (map (catProduct k) [2..])) [1 .. 10^(5 :: Integer) - 1]
        catProduct k n = unDigits 10 $ concatMap ((digits 10) . (* k)) [1..n]
        pan9 x = let ds = digits 10 x in length ds == 9 && all (`elem` ds) [1..9]

answer 39 = pure (showInteger ans)
  where ans = mode (filter (<= maxPerimeter) xs)
        maxPerimeter = 1000
        xs = catMaybes $ do a <- [1..maxPerimeter]
                            b <- [1..maxPerimeter]
                            return $ do c <- intSqrt (square a + square b)
                                        return (a + b + c)

answer 40 = pure (showInt a)
  where a = product (map (d . (10 ^)) ([0..6] :: [Int]))
        d i = (concatMap (digits 10) [1..]) !! (i - 1)

answer 41 = pure (showInteger a)
  where a = head (filter isPrime pandigitalsRev)

answer 42 = do text <- inputText 42
               return (showInt (Problem42.answer text))

answer 43 = pure (showInteger a)
  where a = sum (filter predicate pandigitals)
        predicate i = and (zipWith divides
                            (primes :: [Integer])
                            (Problem43.substrings i))

answer 44 = pure (showInteger a)
  where a = head $ do (n, a) <- zip [0..] pentagonals
                      b <- take n pentagonals
                      guard (isPentagonal (a + b))
                      let c = (a - b)
                      guard (isPentagonal c)
                      return c

answer 45 = pure (showInteger a)
  where a = head (dropWhile (<= 40755) xs)
        xs = filter (\n -> isPentagonal n && isTriangle n) hexagonals

answer 67 = do text <- inputText 67
               return (show (f text))
  where f = TrianglePath.reduceTriangle . TrianglePath.parseTriangle

answer _ = pure "?"
