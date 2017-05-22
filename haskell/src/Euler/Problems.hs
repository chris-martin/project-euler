module Euler.Problems (answers) where

import Euler.Prelude

import Euler.Util.Amicable (amicableNumbers)
import Euler.Util.Arithmetic (divides, factorial, intSqrt, million, square)
import Euler.Util.Collatz (collatzLengths)
import Euler.Util.Decimal (repetendLength)
import Euler.Util.Digit
  (digits, intPalindrome, textDigits, textIntMaybe, unDigits)
import Euler.Util.Fibonacci (fibs)
import Euler.Util.List (countDistinct, maximumOn, mode, sliding)
import Euler.Util.Map (keyWithMaxValue)
import Euler.Util.Pandigital (pandigitalsRev)
import Euler.Util.Prime (countDivisors, isPrime, largestPrimeFactor, primes)

import Euler.Util.FigurateNumbers

import qualified Euler.Util.NumberWords as NumberWords
import qualified Euler.Util.PellEquation as PellEquation
import qualified Euler.Util.TrianglePath as TrianglePath

import qualified Euler.Problems.Problem11 as Problem11
import qualified Euler.Problems.Problem15 as Problem15
import qualified Euler.Problems.Problem19 as Problem19
import qualified Euler.Problems.Problem22 as Problem22
import qualified Euler.Problems.Problem23 as Problem23
import qualified Euler.Problems.Problem31 as Problem31
import qualified Euler.Problems.Problem33 as Problem33
import qualified Euler.Problems.Problem42 as Problem42
import qualified Euler.Problems.Problem43 as Problem43
import qualified Euler.Problems.Problem46 as Problem46
import qualified Euler.Problems.Problem68 as Problem68
import qualified Euler.Problems.Problem69 as Problem69
import qualified Euler.Problems.Problem9 as Problem9

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

-------------------------------------------------------------------------

inputText :: Int -> IO Text
inputText i = TextIO.readFile ("../problems/" <> show i <> "-data.txt")

(~>) :: a -> b -> (a, b)
(~>) a b = (a, b)
infixr 0 ~>

-------------------------------------------------------------------------

-- | Answers to Euler problems.
--
-- The answers to Project Euler problems are all numeric, but the type
-- here is 'String' rather than 'Integer' because in some cases the answer
-- is semantically a "list of digits" rather than an integer, which may
-- be an important distinction if the first digit of an answer is zero.
--
-- Most of the answers are pure, but 'IO' is allowed because some problems
-- involve reading input data from files.
answers :: Map Integer (IO String)
answers = Map.fromList

  [ 1 ~>
    [1..999] &
    filter (\n -> any (`divides` n) ([3, 5] :: [Integer])) &
    sum & showInteger & pure

  , 2 ~>
    fibs &
    takeWhile (< 4 * million) &
    filter even &
    sum & showInteger & pure

  , 3 ~>
    (600851475143 :: Integer) &
    largestPrimeFactor &
    showInteger & pure

  , 4 ~>
    ([1 .. 999] >>= \a -> [1 .. a] <&> \b -> a * b) &
    filter (intPalindrome (10 :: Integer)) &
    maximum & showInteger & pure

  , 5 ~>
    let
      -- greatest powers of primes within the bound
      factors :: [Integer]
      factors = map powerUp (takeWhile (<= bound) primes)

      powerUp n = last (takeWhile (<= bound) (powersOf n))
      powersOf n = map (n^) [(1 :: Int) ..]
      bound = 20
    in
      factors & product & showInteger & pure

  , 6 ~>
    (let xs = [1..100] in square (sum xs) - sum (map square xs)) &
    showInteger & pure

  , 7 ~> primes !! 10000 & showInteger & pure

  , 8 ~>
    inputText 8 <&> \text ->
    text & textDigits & map toInteger & sliding 5 & map product &
    maximum & showInteger

  , 9 ~> Problem9.answer & showInteger & pure

  , 10 ~> primes & takeWhile (< 2 * million) & sum & showInteger & pure

  , 11 ~> inputText 11 <&> \text -> text & Problem11.answer & show

  , 12 ~>
    scanl1 (+) [1..] &
    List.find (\n -> countDivisors n > (500 :: Integer)) &
    fromJust & showInteger & pure

  , 13 ~>
    inputText 13 <&> \text ->
    text &
    Text.lines & mapMaybe textIntMaybe &
    sum & showInteger & take 10

  , 14 ~> [1 .. million] & collatzLengths & keyWithMaxValue & showInteger & pure

  , 15 ~> Problem15.answer & showInteger & pure

  , 16 ~> (2 ^ (1000 :: Int) :: Integer) & digits 10 & sum & showInteger & pure

  , 17 ~>
    [1..1000] &
    fmap (fromIntegral . length . NumberWords.word) & sum & showInteger & pure

  , 18 ~>
    inputText 18 <&> \text ->
    text &
    TrianglePath.parseTriangle & TrianglePath.reduceTriangle &
    showInteger

  , 19 ~> Problem19.answer & showInteger & pure

  , 20 ~> (100 :: Integer) & factorial & digits 10 & sum & showInteger & pure

  , 21 ~> amicableNumbers 9999 & sum & showInteger & pure

  , 22 ~>
    inputText 22 <&> \text ->
    text & Problem22.answer & showInteger

  , 23 ~> Problem23.answer & showInteger & pure

  , 24 ~> pure $ sort (permutations ['0'..'9']) !! (million - 1)

  , 25 ~>
    (fibs :: [Integer]) &
    findIndex (>= 10 ^ (999 :: Integer)) &
    fromJust & fromIntegral & showInteger & pure

  , 26 ~>
    [1..999] &
    maximumOn (repetendLength . (1 /) . fromIntegral) &
    showInteger & pure

  , 27 ~>
    let
      expressions = let x = 999; range = [-x..x]
                    in  liftA2 (,) range range
      apply (a, b) n = n*n + a*n + b
      nrOfPrimes e = (length . takeWhile (isPrime . apply e)) [0..]
    in
      expressions & maximumOn nrOfPrimes & uncurry (*) & showInteger & pure

  , 28 ~>
    [1..500] &
    concatMap (\i -> let j = 2 * i; x = square (j + 1)
                     in  [x, x - j, x - 2*j, x - 3*j]) &
    sum & (+ 1) & showInteger & pure

  , 29 ~>
    let r = [2..100] :: [Integer]
    in  countDistinct [a ^ b | a <- r, b <- r] & showInteger & pure

  , 30 ~>
    let
      maxPowerSum = (* (pow5 9 :: Integer))
      minValue n = 10 ^ (n - 1)
      isFeasible n = maxPowerSum n >= minValue n
      maxNrOfDigits = (last . takeWhile isFeasible) [1..]
      isMagic n = ((== n) . sum . map pow5 . digits 10) n
      pow5 = (^ (5 :: Integer))
    in
      [2 .. (maxPowerSum maxNrOfDigits)] & filter isMagic &
      sum & showInteger & pure

  , 31 ~> Problem31.answer & showInteger & pure

  , 32 ~>
    let
      zs = do
        p <- permutations [1 .. 9 :: Integer]
        let (q, zs') = splitAt 5 p
            z = unDigits 10 zs'
        xl <- [1, 2]
        let (xs, ys) = splitAt xl q
            x = unDigits 10 xs
            y = unDigits 10 ys
        guard $ x * y == z
        pure z
    in
      zs & Set.fromList & sum & showInteger & pure

  , 33 ~> Problem33.answer & showInteger & pure

  , 34 ~>
    let
      maxDigits =
        [1..] &
        takeWhile (\i -> factorial (9 :: Integer) * i >= 10 ^ i) &
        last

      isCurious :: Integer -> Bool
      isCurious n = ((== n) . sum . map factorial . digits (10 :: Integer)) n
    in
      [3 .. 10 ^ maxDigits] & filter isCurious & sum & showInteger & pure

  , 35 ~>
    let
      digitRotations :: Integer -> [Integer]
      digitRotations = map (unDigits 10) . listRotations . digits 10

      listRotations :: [Integer] -> [[Integer]]
      listRotations xs = (map (take l) . take l . List.tails . cycle) xs
        where l = length xs
    in
      ([2 .. million - 1] :: [Integer]) &
      filter (all isPrime . digitRotations) &
      length & fromIntegral & showInteger & pure

  , 36 ~>
    [1 .. million - 1] &
    filter (\x -> intPalindrome ( 2 :: Int) x &&
                  intPalindrome (10 :: Int) x) &
    sum & showInteger & pure

  , 37 ~>
    let
      digitTruncations :: Integer -> [Integer]
      digitTruncations = map (unDigits (10 :: Integer)) . truncations . digits 10

      truncations xs = filter (not . null) (List.tails xs <> List.inits xs)
    in
      [11 :: Integer ..] &
      filter (all isPrime . digitTruncations) &
      take 11 & sum & showInteger & pure

  , 38 ~>
    let
      xs =
        concatMap (\k -> takeWhile (< 10^(9 :: Integer))
        (map (catProduct k) [2 :: Integer ..])) [1 .. 10^(5 :: Integer) - 1]
      catProduct k n =
        unDigits (10 :: Integer) $ concatMap (digits 10 . (* k)) [1..n]
      pan9 x =
        let ds = digits 10 x
        in  length ds == 9 && all (`elem` ds) [1..9 :: Integer]
    in
      xs & filter pan9 & maximum & showInteger & pure

  , 39 ~>
    let
      maxPerimeter = 1000
      xs =
        catMaybes $
        [1 .. maxPerimeter] >>= \a ->
        [1 .. maxPerimeter] <&> \b ->
        intSqrt (square a + square b) <&> \c ->
        a + b + c
    in
      xs & filter (<= maxPerimeter) & mode & showInteger & pure

  , 40 ~>
    let d i = concatMap (digits 10) [1 :: Integer ..] !! (i - 1)
    in  ([0..6] :: [Integer]) & map (d . (10 ^)) & product & showInteger & pure

  , 41 ~> pandigitalsRev & filter isPrime & head & showInteger & pure

  , 42 ~>
    inputText 42 <&> \text ->
    text & Problem42.answer & showInteger

  , 43 ~> Problem43.answer & showInteger & pure

  , 44 ~>
    let
      ans = head $ do
        (n, a) <- zip [0..] pentagonals
        b <- take n pentagonals
        guard $ isPentagonal (a + b)
        let c = a - b
        guard $ isPentagonal c
        pure c
    in
      ans & showInteger & pure

  , 45 ~>
    hexagonals &
    filter (liftA2 (&&) isPentagonal isTriangle) &
    dropWhile (<= 40755) & head & showInteger & pure

  , 46 ~> Problem46.answer & showInteger & pure

  , 66 ~>
    [2..1000] &
    maximumOn PellEquation.fundamentalSolution &
    showInteger & pure

  , 67 ~>
    inputText 67 <&> \text ->
    text &
    TrianglePath.parseTriangle & TrianglePath.reduceTriangle &
    showInteger

  , 68 ~> Problem68.answer & pure

  , 69 ~> (Problem69.answer :: Int) & show & pure

  ]
