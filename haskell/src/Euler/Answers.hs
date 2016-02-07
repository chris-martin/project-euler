{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Euler.Answers
    ( answer1
    , answer2
    , answer3
    , answer4
    , answer5
    , answer6
    , answer7
    , answer8
    , answer9
    , answer10
    , answer11
    , answer12
    , answer13
    , answer14
    , answer15
    , answer16
    , answer17
    , answer18
    , answer19
    , answer20
    , answer21
    , answer22
    , answer23
    , answer24
    , answer25
    , answer26
    , answer27
    , answer28
    , answer29
    , answer30
    , answer67
    ) where

import Data.FileEmbed        ( embedFile )
import Data.Foldable         ( foldMap, toList )
import Data.List             ( findIndex, permutations, sort )
import Data.Map              ( Map )
import Data.Maybe            ( catMaybes, fromJust )
import Data.Numbers.Primes   ( isPrime, primes )
import Data.Sequence         ( replicateM )
import Data.Text             ( Text )
import Data.Text.Encoding    ( decodeUtf8 )

import qualified Data.Char       as Char
import qualified Data.Either     as Either
import qualified Data.Map        as Map
import qualified Data.Map.Strict as Map'
import qualified Data.Sequence   as Seq
import qualified Data.Set        as Set
import qualified Data.Text       as Text
import qualified Data.Text.Read  as TextRead

import Euler.Util.Arithmetic ( divides, factorial, million, square )
import Euler.Util.Amicable   ( amicableNumbers )
import Euler.Util.Collatz    ( collatzLengths )
import Euler.Util.Date       ( monthLength )
import Euler.Util.Decimal    ( repetendLength )
import Euler.Util.Digit      ( intDigits, textDigits )
import Euler.Util.Fibonacci  ( fibs )
import Euler.Util.List       ( countDistinct, maximumOn, sliding, transpose )
import Euler.Util.Map        ( keyWithMaxValue )
import Euler.Util.Prime      ( countDivisors, factorizations, largestPrimeFactor
                             , properDivisorsOfPrimeProduct )

import qualified Euler.Util.TrianglePath as TrianglePath

----------------------------------------------------------------------------

answer1 = show $ sum $ filter f [1..999] where f n = any (`divides` n) [3, 5]

answer2 = show $ sum $ filter even $ takeWhile (< 4 * million) fibs

answer3 = show $ largestPrimeFactor 600851475143

answer4 = show $ maximum $ filter isPalindrome $ map product $ pairsOf [1..999] where
    isPalindrome i = s == reverse s where s = show i
    pairsOf xs = map toList $ replicateM 2 xs

answer5 = show $ product factors where

    -- greatest powers of primes within the bound
    factors = map powerUp $ takeWhile (<= bound) primes

    powerUp n = last $ takeWhile (<= bound) $ powersOf n
    powersOf n = map (n^) [ 1 :: Int .. ]
    bound = 20

answer6 = show $ (square $ sum xs) - (sum $ map square xs) where xs = [1..100]

answer7 = show $ primes !! 10000

answer8 = show $ maximum $ map product $ sliding 5 $ map toInteger digits where
    digits = textDigits inputText8

answer9 = show $ tupleProduct triple where
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

answer10 = show $ sum $ takeWhile (< 2 * million) primes

answer11 = show $ maximum $ map product groups where

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
    grid = map parseLine $ Text.lines inputText11 where
        parseLine = map fst . Either.rights . (map TextRead.decimal) . Text.words

answer12 = show $ head $ filter (\n -> countDivisors n > 500) $ scanl1 (+) [1..]

answer13 = take 10 $ show $ sum numbers where
    numbers :: [Integer]
    numbers = parseNumbers inputText13 where
        parseNumbers = Either.rights . map parseLine . Text.lines
        parseLine = (fmap fst) . TextRead.decimal

answer14 = show $ keyWithMaxValue $ collatzLengths [1 .. million]

answer15 = show $ fromJust $ Map.lookup d $ countPaths Map.empty [d] where
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

answer16 = show $ sum $ intDigits $ 2 ^ 1000

answer17 = show $ sum $ map length (_1_1000 :: [String]) where

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

answer18 = show $ TrianglePath.reduceTriangle $ TrianglePath.parseTriangle $ inputText18

answer19 = show $ length $ filter isMatchingDate datesWithWeekday where

    dates = do year  <- [1900 .. 2000]
               month <- [1 .. 12]
               day   <- [1 .. monthLength year month]
               return (year, month, day)

    datesWithWeekday = zip dates $ cycle [1 .. 7]

    isMatchingDate ((year, month, day), weekday) =
        year /= 1900 && day == 1 && weekday == 7

answer20 = show $ sum $ intDigits $ factorial 100

answer21 = show $ sum $ amicableNumbers 9999

answer22 = show $ sum scores where
    names = sort $ parseNames inputText22
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

answer23 = show $ sum $ (Set.fromList [1..max]) Set.\\ (Set.fromList xs) where

    max = 28123

    divisorSums = fmap (sum . properDivisorsOfPrimeProduct) (factorizations max)
    d = (divisorSums Map.!)

    abundants = Seq.fromList $ filter (\n -> d n > n) [2..max]
    ab = (abundants `Seq.index`)
    l = (length abundants) - 1

    xs = do x <- [0..l]
            y <- [x..l]
            return $ (ab x) + (ab y)

answer24 = (sort $ permutations ['0'..'9']) !! (million - 1)

answer25 = show $ fromJust $ findIndex (>= x) fibs where x = 10 ^ 999

answer26 = show (i :: Integer) where
    i = maximumOn f [1..999]
    f = repetendLength . (1 /) . fromIntegral

answer27 = show $ (uncurry (*)) $ maximumOn nrOfPrimes expressions where
    expressions = do a <- range; b <- range; return (a, b)
                  where x = 999; range = [-x..x]
    apply (a, b) n = n*n + a*n + b
    nrOfPrimes e = length $ takeWhile (isPrime . (apply e)) [0..]

answer28 = show $ (+) 1 $ sum $ concatMap f [1..500] where
    f i = let j = 2 * i
              x = square $ j + 1
          in  [x, x - j, x - 2*j, x - 3*j]

answer29 = show $ countDistinct $ [a ^ b | a <- r, b <- r] where r = [2..100]

answer30 = show $ sum $ filter isMagic [2 .. (maxPowerSum maxNrOfDigits)] where
    maxPowerSum = (* (9 ^ 5))
    minValue n = 10 ^ (n - 1)
    isFeasible n = maxPowerSum n >= minValue n
    maxNrOfDigits = last $ takeWhile isFeasible [1..]
    isMagic n = (==) n $ sum $ map (^ 5) $ intDigits n

answer67 = show $ TrianglePath.reduceTriangle $ TrianglePath.parseTriangle $ inputText67

----------------------------------------------------------------------------

inputText8   :: Text
inputText11  :: Text
inputText13  :: Text
inputText18  :: Text
inputText22  :: Text
inputText67  :: Text

inputText8   = decodeUtf8 $(embedFile "../problems/8-data.txt")
inputText11  = decodeUtf8 $(embedFile "../problems/11-data.txt")
inputText13  = decodeUtf8 $(embedFile "../problems/13-data.txt")
inputText18  = decodeUtf8 $(embedFile "../problems/18-data.txt")
inputText22  = decodeUtf8 $(embedFile "../problems/22-data.txt")
inputText67  = decodeUtf8 $(embedFile "../problems/67-data.txt")

----------------------------------------------------------------------------

answer1   :: String
answer2   :: String
answer3   :: String
answer4   :: String
answer5   :: String
answer6   :: String
answer7   :: String
answer8   :: String
answer9   :: String
answer10  :: String
answer11  :: String
answer12  :: String
answer13  :: String
answer14  :: String
answer15  :: String
answer16  :: String
answer17  :: String
answer18  :: String
answer19  :: String
answer20  :: String
answer21  :: String
answer22  :: String
answer23  :: String
answer24  :: String
answer25  :: String
answer26  :: String
answer27  :: String
answer28  :: String
answer29  :: String
answer30  :: String
answer67  :: String
