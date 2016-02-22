{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Euler.Answers (answer) where

import Data.Digits           ( digits, unDigits )
import Data.Foldable         ( foldMap, toList )
import Data.List             ( findIndex, permutations, sort )
import Data.Map              ( Map )
import Data.Maybe            ( catMaybes, fromJust )
import Data.Numbers.Primes   ( isPrime, primes )
import Data.Ratio            ( (%), numerator, denominator )
import Data.Sequence         ( replicateM )
import Data.Text             ( Text )
import Data.Text.Encoding    ( decodeUtf8 )

import qualified Data.Char       as Char
import qualified Data.Either     as Either
import qualified Data.List       as List
import qualified Data.Map        as Map
import qualified Data.Map.Strict as Map'
import qualified Data.Sequence   as Seq
import qualified Data.Set        as Set
import qualified Data.Text       as Text
import qualified Data.Text.Read  as TextRead
import qualified Data.Text.IO    as TextIO

import Euler.Util.Arithmetic ( divides, factorial, factorials, intSqrt, million, square )
import Euler.Util.Amicable   ( amicableNumbers )
import Euler.Util.Collatz    ( collatzLengths )
import Euler.Util.Date       ( monthLength )
import Euler.Util.Decimal    ( repetendLength )
import Euler.Util.Digit      ( intPalindrome, textDigits )
import Euler.Util.Fibonacci  ( fibs )
import Euler.Util.List       ( countDistinct, maximumOn, mode, sliding, transpose )
import Euler.Util.Map        ( keyWithMaxValue )
import Euler.Util.Prime      ( countDivisors, factorizations, largestPrimeFactor
                             , properDivisorsOfPrimeProduct )

import qualified Euler.Util.TrianglePath as TrianglePath

----------------------------------------------------------------------------

inputText :: Int -> IO Text
inputText i = TextIO.readFile $ "../problems/" ++ (show i) ++ "-data.txt"

----------------------------------------------------------------------------

answer :: Int -> IO String

answer 1 = pure $ show $ sum $ filter f [1..999] where f n = any (`divides` n) [3, 5]

answer 2 = pure $ show $ sum $ filter even $ takeWhile (< 4 * million) fibs

answer 3 = pure $ show $ largestPrimeFactor 600851475143

answer 4 = pure $ show $ maximum $ filter (intPalindrome 10) $ map product $ pairsOf [1..999] where
    pairsOf xs = map toList $ replicateM 2 xs

answer 5 = pure $ show $ product factors where

    -- greatest powers of primes within the bound
    factors = map powerUp $ takeWhile (<= bound) primes

    powerUp n = last $ takeWhile (<= bound) $ powersOf n
    powersOf n = map (n^) [ 1 :: Int .. ]
    bound = 20

answer 6 = pure $ show $ (square $ sum xs) - (sum $ map square xs) where xs = [1..100]

answer 7 = pure $ show $ primes !! 10000

answer 8 = fmap f $ inputText 8 where
    f = show . maximum . (map product) . (sliding 5) . (map toInteger) . textDigits

answer 9 = pure $ show $ tupleProduct triple where
    tupleProduct (a, b, c) = a * b * c

    -- the pythagorean triple the question asks us to find
    triple :: (Integer, Integer, Integer)
    triple = head $ filter isPythagorean triples where

        -- candidate triples that aren't necessarily pythagorean
        triples = map completeTriple $ pairsOf [1..magicNumber]
        completeTriple (a, b) = (a, b, magicNumber - a - b)
        magicNumber = 1000

    isPythagorean :: (Integer, Integer, Integer) -> Bool
    isPythagorean (a, b, c) = a*a + b*b == c*c

    pairsOf :: [a] -> [(a, a)]
    pairsOf xs = foldMap (\a -> map (\b -> (a, b)) xs) xs

answer 10 = pure $ show $ sum $ takeWhile (< 2 * million) primes

answer 11 = fmap f $ inputText 11 where
  f t = show $ maximum $ map product groups where

    groups = foldMap (sliding 4) (concat grids)

    grids = [ grid
            , transpose grid
            , shift grid
            , (shift . reverseRows) grid ]

    shift rows = transpose $ map shiftRow $ zip rows [0..] where
        shiftRow (row, i) = (replicate i 0) ++ row ++
                            (replicate (2 * (length row)) 0)

    reverseRows rows = map reverse rows

    grid :: [[Integer]]
    grid = map parseLine $ Text.lines t where
        parseLine = map fst . Either.rights . (map TextRead.decimal) . Text.words

answer 12 = pure $ show $ head $ filter (\n -> countDivisors n > 500) $ scanl1 (+) [1..]

answer 13 = fmap f $ inputText 13 where
  f t = take 10 $ show $ sum numbers where
    numbers :: [Integer]
    numbers = parseNumbers t where
        parseNumbers = Either.rights . map parseLine . Text.lines
        parseLine = (fmap fst) . TextRead.decimal

answer 14 = pure $ show $ keyWithMaxValue $ collatzLengths [1 .. million]

answer 15 = pure $ show $ fromJust $ Map.lookup d $ countPaths Map.empty [d] where
    d = (20, 20)

    -- When the stack is empty, we're done.
    countPaths counts [] = counts
    countPaths counts stack@((x, y):restOfStack)
        -- If the stack head's count is already known, just pop it off.
        | Map.member (x, y) counts = countPaths counts restOfStack
        -- If either coordinate is zero, there's only one path.
        -- Add it to the counts and pop it off the stack,
        | x == 0 || y == 0 = countPaths (Map.insert (x, y) 1 counts) restOfStack
        -- If both of the adjacencies' counts are known, then this count
        -- is their sum. Add it to the counts and pop it off the stack,
        | null unknownAdjacencies =
            let c = sum $ catMaybes $ map (flip Map.lookup counts) adjacencies
            in countPaths (Map.insert (x, y) c counts) restOfStack
        -- There are some unknown adjacencies. Add them to the stack.
        | otherwise = countPaths counts (unknownAdjacencies ++ stack)
        where adjacencies = [(x-1, y), (x, y-1)]
              unknownAdjacencies = filter (flip Map.notMember counts) adjacencies

answer 16 = pure $ show $ sum $ digits 10 $ 2 ^ 1000

answer 17 = pure $ show $ sum $ map length (_1_1000 :: [String]) where

    _1_9   = [ "one", "two", "three", "four", "five"
             , "six", "seven", "eight", "nine" ]

    _10_19 = [ "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen"
             , "sixteen", "seventeen", "eighteen", "nineteen" ]

    _20_99 = do a <- [ "twenty", "thirty", "forty", "fifty"
                     , "sixty", "seventy", "eighty", "ninety" ]
                b <- "" : _1_9
                return (a ++ b)

    _1_99 = _1_9 ++ _10_19 ++ _20_99

    _100_999 = do a <- map (++ "hundred") _1_9
                  b <- "" : map ("and" ++) _1_99
                  return (a ++ b)

    _1_1000 = _1_99 ++ _100_999 ++ [ "onethousand" ]

answer 18 = fmap (show . TrianglePath.reduceTriangle . TrianglePath.parseTriangle) (inputText 18)

answer 19 = pure $ show $ length $ filter isMatchingDate datesWithWeekday where

    dates = do year  <- [1900 .. 2000]
               month <- [1 .. 12]
               day   <- [1 .. monthLength year month]
               return (year, month, day)

    datesWithWeekday = zip dates $ cycle [1 .. 7]

    isMatchingDate ((year, month, day), weekday) =
        year /= 1900 && day == 1 && weekday == 7

answer 20 = pure $ show $ sum $ digits 10 $ factorial 100

answer 21 = pure $ show $ sum $ amicableNumbers 9999

answer 22 = fmap f $ inputText 22 where
  f t = show $ sum scores where
    names = sort $ parseNames t
    scores = zipWith (*) [1..] $ map wordScore names

    -- "COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53"
    wordScore :: Text -> Integer
    wordScore = sum . (map $ fromIntegral . letterScore) . Text.unpack

    -- A = 1, B = 2, etc.
    letterScore :: Char -> Int
    letterScore c = (Char.ord c) - (Char.ord 'A') + 1

    parseNames :: Text -> [Text]
    parseNames = (map parseName) . (Text.splitOn ",") . Text.toUpper where
        parseName = unquote . Text.strip
        unquote = (Text.dropWhile isQ) . (Text.dropWhileEnd isQ)
        isQ = (== '"')

answer 23 = pure $ show $ sum $ (Set.fromList [1..max]) Set.\\ (Set.fromList xs) where

    max = 28123

    divisorSums = fmap (sum . properDivisorsOfPrimeProduct) (factorizations max)
    d = (divisorSums Map.!)

    abundants = Seq.fromList $ filter (\n -> d n > n) [2..max]
    ab = (abundants `Seq.index`)
    l = (length abundants) - 1

    xs = do x <- [0..l]
            y <- [x..l]
            return $ (ab x) + (ab y)

answer 24 = pure $ (sort $ permutations ['0'..'9']) !! (million - 1)

answer 25 = pure $ show $ fromJust $ findIndex (>= x) fibs where x = 10 ^ 999

answer 26 = pure $ show (i :: Integer) where
    i = maximumOn f [1..999]
    f = repetendLength . (1 /) . fromIntegral

answer 27 = pure $ show $ (uncurry (*)) $ maximumOn nrOfPrimes expressions where
    expressions = do a <- range; b <- range; return (a, b)
                  where x = 999; range = [-x..x]
    apply (a, b) n = n*n + a*n + b
    nrOfPrimes e = length $ takeWhile (isPrime . (apply e)) [0..]

answer 28 = pure $ show $ (+) 1 $ sum $ concatMap f [1..500] where
    f i = let j = 2 * i
              x = square $ j + 1
          in  [x, x - j, x - 2*j, x - 3*j]

answer 29 = pure $ show $ countDistinct $ [a ^ b | a <- r, b <- r] where r = [2..100]

answer 30 = pure $ show $ sum $ filter isMagic [2 .. (maxPowerSum maxNrOfDigits)] where
    maxPowerSum = (* (9 ^ 5))
    minValue n = 10 ^ (n - 1)
    isFeasible n = maxPowerSum n >= minValue n
    maxNrOfDigits = last $ takeWhile isFeasible [1..]
    isMagic n = (==) n $ sum $ map (^ 5) $ digits 10 n

answer 31 = pure $ show $ count [] where

    denominations = [1, 2, 5, 10, 20, 50, 100, 200]
    target = 200
    pence = sum . (zipWith (*) denominations)

    count base | p == target                         = 1
               | p >  target                         = 0
               | length base == length denominations = 0
               | otherwise                           = recurse
        where p = pence base
              recurse = sum $ map (\n -> count $ base ++ [n]) $
                        [0 .. (target - p) `div` (denominations !! (length base))]

answer 32 = pure $ show $ sum $ Set.fromList $ do
    p <- permutations [1..9]
    let (q, zs) = splitAt 5 p
        z = unDigits 10 zs
    xl <- [1, 2]
    let (xs, ys) = splitAt xl q
        x = unDigits 10 xs
        y = unDigits 10 ys
    _ <- if x * y == z then [True] else []
    return z

answer 33 = pure $ show $ denominator $ product $ map (uncurry (%)) specialFractions where

    specialFractions = filter isCurious $ do c <- [10    .. 99]
                                             d <- [c + 1 .. 99]
                                             return (c, d)

    isCurious (c, d) = any f $ do a <- permutations $ digits 10 c
                                  b <- permutations $ digits 10 d
                                  return (a, b) where
                       f ([a0, a1], [b0, b1]) = a0 /= 0   &&
                                                b1 /= 0   &&
                                                a0 == b0  &&
                                                c % d == a1 % b1

answer 34 = pure $ show $ sum $ filter isCurious [3 .. 10 ^ maxDigits] where
    maxDigits = last $ takeWhile f [1..] where
        f i = (factorial 9) * i >= 10 ^ i
    isCurious :: Integer -> Bool
    isCurious n = (==) n $ sum $ map factorial $ digits 10 n

answer 35 = pure $ show $ length $ filter ((all isPrime) . digitRotations) [2 .. million - 1] where
    digitRotations x = map (unDigits 10) $ listRotations $ digits 10 x
    listRotations xs = map (take l) $ take l $ List.tails $ cycle xs where l = length xs

answer 36 = pure $ show $ sum $ filter f [1 .. million - 1] where
    f x = intPalindrome 2 x && intPalindrome 10 x

answer 37 = pure $ show $ sum $ take 11 $ filter ((all isPrime) . digitTruncations) [11..] where
    digitTruncations = (map $ unDigits 10) . truncations . (digits 10)
    truncations xs = filter (not . null) $ List.tails xs ++ List.inits xs

answer 38 = pure $ show $ maximum $ filter pan9 xs where
    xs = concatMap (\k -> takeWhile (< 10^9) $ map (catProduct k) [2..]) [1 .. 10^5 - 1]
    catProduct k n = unDigits 10 $ concatMap ((digits 10) . (* k)) [1..n]
    pan9 x = let ds = digits 10 x in length ds == 9 && all (`elem` ds) [1..9]

answer 39 = pure $ show $ mode $ filter (<= maxPerimeter) xs where
    maxPerimeter = 1000
    xs = catMaybes $ do a <- [1..maxPerimeter]
                        b <- [1..maxPerimeter]
                        return $ fmap (+ (a+b)) $ intSqrt (square a + square b)

answer 40 = pure $ show $ product $ map (d . (10 ^)) [0..6] where
    d i = ((concatMap (digits 10) [1..]) !! (i - 1))

answer 41 = pure $ show $ head $ filter isPrime pandigitals where
    pandigitals = concatMap pandigitalsOfLength $ [9, 8 .. 1]
    pandigitalsOfLength n = map (unDigits 10) $ reverse $ sort $ permutations [n, n-1 .. 1]

answer 42 = fmap f $ (inputText 42) where
  f t = show $ length $ filter isTriangleWord words where
    triangles = map (\n -> (n * (n + 1)) `div` 2) [1..]
    isTriangleNum v = elem v $ takeWhile (<= v) triangles
    isTriangleWord = isTriangleNum . wordValue

    words = Text.splitOn "," $ Text.filter (/= '"') t

    -- wordValue "Sky" = 19 + 11 + 25 = 55
    wordValue = sum . (map letterValue) . Text.unpack . Text.toUpper

    -- A = 1, B = 2, etc.
    letterValue c = (Char.ord c) - (Char.ord 'A') + 1

answer 67 = fmap (show . TrianglePath.reduceTriangle . TrianglePath.parseTriangle) (inputText 67)
