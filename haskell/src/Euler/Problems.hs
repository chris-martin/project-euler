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
import Euler.Util.Pentagonal ( pentagonals, isPentagonal )
import Euler.Util.Prime      ( countDivisors, largestPrimeFactor )

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
               , fromIntegral, fst, last, length, head, map
               , maximum, not, null, product, pure, return, scanl1
               , splitAt, sum, take, takeWhile, toInteger, uncurry
               , zip, zipWith
               , Bool(..), Int, Integer, Integral, Show(..), String, IO )

import Control.Monad         ( guard )
import Data.Digits           ( digits, unDigits )
import Data.Foldable         ( toList )
import Data.List             ( findIndex, permutations, sort )
import Data.Maybe            ( catMaybes, fromJust )
import Data.Numbers.Primes   ( isPrime, primes )
import Data.Sequence         ( replicateM )
import Data.Text             ( Text )

import qualified Data.Either     as Either
import qualified Data.List       as List
import qualified Data.Set        as Set
import qualified Data.Text       as Text
import qualified Data.Text.IO    as TextIO

inputText :: Int -> IO Text
inputText i = TextIO.readFile ("../problems/" ++ (show i) ++ "-data.txt")

showInteger :: Integer -> String
showInteger = show

showInt :: Int -> String
showInt = show

answer :: Integral a => a -> IO String

answer 1 = (pure . showInteger . sum . (filter f)) [1..999]
  where f n = any (`divides` n) ([3, 5] :: [Integer])

answer 2 = (pure . showInteger . sum . (filter even) . (takeWhile (< 4 * million))) fibs

answer 3 = (pure . showInteger . largestPrimeFactor) (600851475143 :: Integer)

answer 4 =
  (pure . showInteger . maximum . (filter (intPalindrome 10)) . (map product)) (pairsOf [1..999])
  where pairsOf xs = map toList (replicateM 2 xs)

answer 5 = (pure . showInteger . product) factors
  where
    -- greatest powers of primes within the bound
    factors :: [Integer]
    factors = map powerUp (takeWhile (<= bound) primes)

    powerUp n = last (takeWhile (<= bound) (powersOf n))
    powersOf n = map (n^) [(1 :: Int) ..]
    bound = 20

answer 6 = (pure . showInteger) ((square (sum xs)) - (sum (map square xs)))
  where xs = [1..100]

answer 7 = (pure . showInteger) (primes !! 10000)

answer 8 =
  do text <- inputText 8
     return (f text)
  where f = showInteger . maximum . (map product) . (sliding 5) . (map toInteger) . textDigits

answer 9 = (pure . showInteger) Problem9.answer

answer 10 = (pure . showInteger . sum . (takeWhile (< 2 * million))) primes

answer 11 = do text <- inputText 11
               return ((show . Problem11.answer) text)

answer 12 = (pure . showInteger) i
  where
    i = (fromJust . (List.find f)) (scanl1 (+) [1..])
    f n = countDivisors n > (500 :: Integer)

answer 13 =
  do
    text <- inputText 13
    return (f text)
  where
    f = (take 10) . showInteger . sum . parseNumbers
    parseNumbers = catMaybes . map textIntMaybe . Text.lines

answer 14 = (pure . showInteger . keyWithMaxValue . collatzLengths) [1 .. million]

answer 15 = (pure . showInteger) Problem15.answer

answer 16 = (pure . showInteger . sum . (digits 10)) (2 ^ (1000 :: Integer))

answer 17 = (pure . show . sum) (map (length . NumberWords.word) [1..1000])

answer 18 = do
  text <- inputText 18
  (return . show . TrianglePath.reduceTriangle
                 . TrianglePath.parseTriangle) text

answer 19 = (pure . showInt . length) (filter isMatchingDate datesWithWeekday)
  where
    dates = do year  <- [1900 .. 2000] :: [Int]
               month <- [1 .. 12]      :: [Int]
               day   <- [1 .. monthLength year month]
               return (year, month, day)

    datesWithWeekday = zip dates (cycle ([1 .. 7] :: [Int]))

    isMatchingDate ((year, _month, day), weekday) =
        year /= 1900 && day == 1 && weekday == 7

answer 20 = (pure . showInteger . sum . (digits 10) . factorial) (100 :: Integer)

answer 21 = (pure . showInteger . sum) (amicableNumbers 9999)

answer 22 = do text <- inputText 22
               return (showInteger (Problem22.answer text))

answer 23 = pure (showInteger Problem23.answer)

answer 24 = pure ((sort (permutations ['0'..'9'])) !! (million - 1))

answer 25 = (pure . showInt . fromJust . (findIndex (>= x))) (fibs :: [Integer])
  where x = 10 ^ (999 :: Integer)

answer 26 = pure (showInteger i)
  where
    i = maximumOn f [1..999]
    f = repetendLength . (1 /) . fromIntegral

answer 27 = (pure . showInt . (uncurry (*)) . (maximumOn nrOfPrimes)) expressions
  where
    expressions = do a <- range; b <- range; return (a, b)
                  where x = 999; range = [-x..x]
    apply (a, b) n = n*n + a*n + b
    nrOfPrimes e = (length . (takeWhile (isPrime . (apply e)))) [0..]

answer 28 = (pure . showInteger . (+ 1) . sum) (concatMap f [1..500]) where
    f i = let j = 2 * i
              x = square (j + 1)
          in  [x, x - j, x - 2*j, x - 3*j]

answer 29 = (pure . showInt . countDistinct) [a ^ b | a <- r, b <- r]
  where r = [2..100] :: [Integer]

answer 30 = (pure . showInteger . sum) (filter isMagic [(2 :: Integer) .. (maxPowerSum maxNrOfDigits)])
  where
    maxPowerSum = (* (pow5 9))
    minValue n = 10 ^ (n - 1)
    isFeasible n = maxPowerSum n >= minValue n
    maxNrOfDigits = (last . (takeWhile isFeasible)) [1..]
    isMagic n = ((== n) . sum . (map pow5) . (digits 10)) n
    pow5 = (^ (5 :: Integer))

answer 31 = pure (showInteger Problem31.answer)

answer 32 = (pure . showInteger . sum . Set.fromList) $ do
    p <- permutations [1..9]
    let (q, zs) = splitAt 5 p
        z = unDigits 10 zs
    xl <- [1, 2]
    let (xs, ys) = splitAt xl q
        x = unDigits 10 xs
        y = unDigits 10 ys
    guard (x * y == z)
    return z

answer 33 = pure (showInteger Problem33.answer)

answer 34 = (pure . showInteger . sum . (filter isCurious)) [3 .. 10 ^ maxDigits]
  where
    maxDigits = last (takeWhile f [1..])
      where f i = (factorial (9 :: Integer)) * i >= 10 ^ i
    isCurious :: Integer -> Bool
    isCurious n = ((== n) . sum . (map factorial) . (digits 10)) n

answer 35 = (pure . showInt . length . (filter ((all isPrime) . digitRotations)))
            ([2 .. million - 1] :: [Integer])
  where
    digitRotations x = ((map (unDigits 10)) . listRotations . (digits 10)) x
    listRotations xs = ((map (take l)) . (take l) . List.tails . cycle) xs where l = length xs

answer 36 = (pure . showInteger . sum . (filter f)) [1 .. million - 1]
  where f x = intPalindrome 2 x && intPalindrome 10 x

answer 37 = (pure . showInteger . sum . (take 11) . (filter ((all isPrime) . digitTruncations))) [11..]
  where
    digitTruncations = (map (unDigits 10)) . truncations . (digits 10)
    truncations xs = filter (not . null) (List.tails xs ++ List.inits xs)

answer 38 = (pure . showInteger . maximum . (filter pan9)) xs
  where
    xs = concatMap (\k -> takeWhile (< 10^(9 :: Integer)) (map (catProduct k) [2..])) [1 .. 10^(5 :: Integer) - 1]
    catProduct k n = unDigits 10 $ concatMap ((digits 10) . (* k)) [1..n]
    pan9 x = let ds = digits 10 x in length ds == 9 && all (`elem` ds) [1..9]

answer 39 = (pure . showInteger . mode . (filter (<= maxPerimeter))) xs
  where
    maxPerimeter = 1000
    xs = catMaybes $ do a <- [1..maxPerimeter]
                        b <- [1..maxPerimeter]
                        return $ do c <- intSqrt (square a + square b)
                                    return (a + b + c)

answer 40 = (pure . showInt . product . (map (d . (10 ^)))) ([0..6] :: [Int])
  where
    d :: Int -> Int
    d i = (concatMap (digits 10) [1..]) !! (i - 1)

answer 41 = (pure . showInteger . head . (filter isPrime)) pandigitalsRev

answer 42 = do text <- inputText 42
               return (showInt (Problem42.answer text))

answer 43 = (pure . showInteger . sum . (filter predicate)) pandigitals
  where predicate i = and (zipWith divides (primes :: [Integer]) (Problem43.substrings i))

answer 44 = (pure . showInteger . head) $ do
    (n, a) <- zip [0..] pentagonals
    b <- take n pentagonals
    guard (isPentagonal (a + b))
    let c = (a - b)
    guard (isPentagonal c)
    return c

answer 67 = do
  text <- inputText 67
  (return . show . TrianglePath.reduceTriangle
                 . TrianglePath.parseTriangle) text

answer _ = pure "?"