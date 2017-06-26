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
    List.filter (\n -> List.any (`divides` n) ([3, 5] :: [Integer])) &
    List.sum & showInteger & pure

  , 2 ~>
    fibs &
    List.takeWhile (< 4 * million) &
    List.filter even &
    List.sum & showInteger & pure

  , 3 ~>
    (600851475143 :: Integer) &
    largestPrimeFactor &
    showInteger & pure

  , 4 ~>
    ([1 .. 999] >>= \a -> [1 .. a] <&> \b -> a * b) &
    List.filter (intPalindrome (10 :: Integer)) &
    List.maximum & showInteger & pure

  , 5 ~>
    let
      -- greatest powers of primes within the bound
      factors :: [Integer]
      factors = fmap powerUp (List.takeWhile (<= bound) primes)

      powerUp n = List.last (List.takeWhile (<= bound) (powersOf n))
      powersOf n = fmap (n^) [(1 :: Int) ..]
      bound = 20
    in
      factors & List.product & showInteger & pure

  , 6 ~>
    (let xs = [1..100] in square (List.sum xs) - List.sum (fmap square xs)) &
    showInteger & pure

  , 7 ~> primes List.!! 10000 & showInteger & pure

  , 8 ~>
    inputText 8 <&> \text ->
    text & textDigits & fmap toInteger & sliding 5 & fmap List.product &
    List.maximum & showInteger

  , 9 ~> Problem9.answer & showNatural & pure

  , 10 ~>
    primes & List.takeWhile (< 2 * million) &
    List.sum & showInteger & pure

  , 11 ~> inputText 11 <&> \text -> text & Problem11.answer & show

  , 12 ~>
    List.scanl1 (+) [1..] &
    List.find (\n -> countDivisors n > (500 :: Integer)) &
    fromJust & showInteger & pure

  , 13 ~>
    inputText 13 <&> \text ->
    text &
    Text.lines & mapMaybe textIntMaybe &
    List.sum & showInteger & List.take 10

  , 14 ~>
    [1 .. million] & collatzLengths & keyWithMaxValue &
    showNatural & pure

  , 15 ~> Problem15.answer & showInteger & pure

  , 16 ~>
    (2 ^ (1000 :: Int) :: Integer) & digits 10 & List.sum &
    showInteger & pure

  , 17 ~>
    [1..1000] & foldMap NumberWords.word &
    fmap (NumberWords.getLetterCount >>> showNatural)

  , 18 ~>
    inputText 18 <&> \text ->
    text &
    TrianglePath.parseTriangle & TrianglePath.reduceTriangle &
    showInteger

  , 19 ~> Problem19.answer & showInteger & pure

  , 20 ~>
    (100 :: Integer) & factorial & digits 10 &
    List.sum & showInteger & pure

  , 21 ~> amicableNumbers 9999 & List.sum & showInteger & pure

  , 22 ~>
    inputText 22 <&> \text ->
    text & Problem22.answer & showInteger

  , 23 ~> Problem23.answer & showInteger & pure

  , 24 ~> pure $ sort (permutations ['0'..'9']) List.!! (million - 1)

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
      nrOfPrimes e = (List.length . List.takeWhile (isPrime . apply e)) [0..]
    in
      expressions & maximumOn nrOfPrimes & uncurry (*) & showInteger & pure

  , 28 ~>
    [1..500] &
    foldMap (\i -> let j = 2 * i; x = square (j + 1)
                     in  [x, x - j, x - 2*j, x - 3*j]) &
    List.sum & (+ 1) & showInteger & pure

  , 29 ~>
    let r = [2..100] :: [Integer]
    in  countDistinct [a ^ b | a <- r, b <- r] & showInteger & pure

  , 30 ~>
    let
      maxPowerSum = (* (pow5 9 :: Integer))
      minValue n = 10 ^ (n - 1)
      isFeasible n = maxPowerSum n >= minValue n
      maxNrOfDigits = (List.last . List.takeWhile isFeasible) [1..]
      isMagic n = ((== n) . List.sum . fmap pow5 . digits 10) n
      pow5 = (^ (5 :: Integer))
    in
      [2 .. (maxPowerSum maxNrOfDigits)] & List.filter isMagic &
      List.sum & showInteger & pure

  , 31 ~> Problem31.answer & showNatural & pure

  , 32 ~>
    let
      zs = do
        p <- permutations [1 .. 9 :: Integer]
        let (q, zs') = List.splitAt 5 p
            z = unDigits 10 zs'
        xl <- [1, 2]
        let (xs, ys) = List.splitAt xl q
            x = unDigits 10 xs
            y = unDigits 10 ys
        guard $ x * y == z
        pure z
    in
      zs & Set.fromList & List.sum & showInteger & pure

  , 33 ~> Problem33.answer & showInteger & pure

  , 34 ~>
    let
      maxDigits =
        [1..] &
        List.takeWhile (\i -> factorial (9 :: Integer) * i >= 10 ^ i) &
        List.last

      isCurious :: Integer -> Bool
      isCurious n =
        ((== n) . List.sum . fmap factorial . digits (10 :: Integer)) n
    in
      [3 .. 10 ^ maxDigits] & List.filter isCurious &
      List.sum & showInteger & pure

  , 35 ~>
    let
      digitRotations :: Integer -> [Integer]
      digitRotations = fmap (unDigits 10) . listRotations . digits 10

      listRotations :: [Integer] -> [[Integer]]
      listRotations xs =
          (fmap (List.take l) . List.take l . List.tails . List.cycle) xs
        where
          l = List.length xs
    in
      ([2 .. million - 1] :: [Integer]) &
      List.filter (List.all isPrime . digitRotations) &
      List.length & fromIntegral & showInteger & pure

  , 36 ~>
    [1 .. million - 1] &
    List.filter (\x -> intPalindrome ( 2 :: Int) x &&
                       intPalindrome (10 :: Int) x) &
    List.sum & showInteger & pure

  , 37 ~>
    let
      digitTruncations :: Integer -> [Integer]
      digitTruncations =
        fmap (unDigits (10 :: Integer)) . truncations . digits 10

      truncations xs =
        List.filter (not . List.null) (List.tails xs <> List.inits xs)
    in
      [11 :: Integer ..] &
      List.filter (List.all isPrime . digitTruncations) &
      List.take 11 & List.sum & showInteger & pure

  , 38 ~>
    let
      xs =
        foldMap (\k -> List.takeWhile (< 10^(9 :: Integer))
        (fmap (catProduct k) [2 :: Integer ..])) [1 .. 10^(5 :: Integer) - 1]
      catProduct k n =
        unDigits (10 :: Integer) $ foldMap (digits 10 . (* k)) [1..n]
      pan9 x =
        let ds = digits 10 x
        in  List.length ds == 9 && List.all (`List.elem` ds) [1..9 :: Integer]
    in
      xs & List.filter pan9 & List.maximum & showInteger & pure

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
      xs & List.filter (<= maxPerimeter) & mode & showInteger & pure

  , 40 ~>
    let d i = foldMap (digits 10) [1 :: Integer ..] List.!! (i - 1)
    in  ([0..6] :: [Integer]) & fmap (d . (10 ^)) &
        List.product & showInteger & pure

  , 41 ~>
    pandigitalsRev & List.filter isPrime & List.head &
    showInteger & pure

  , 42 ~>
    inputText 42 <&> \text ->
    text & Problem42.answer & showInteger

  , 43 ~> Problem43.answer & showInteger & pure

  , 44 ~>
    let
      ans = List.head $ do
        (n, a) <- List.zip [0..] pentagonals
        b <- List.take n pentagonals
        guard $ isPentagonal (a + b)
        let c = a - b
        guard $ isPentagonal c
        pure c
    in
      ans & showInteger & pure

  , 45 ~>
    hexagonals &
    List.filter (liftA2 (&&) isPentagonal isTriangle) &
    List.dropWhile (<= 40755) & List.head & showInteger & pure

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
