{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module EulerTest.Answers
    ( fastTest
    , answerTest
    ) where

import Test.HUnit ((@?=))
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Data.FileEmbed     ( embedFile )
import Data.Text.Encoding ( decodeUtf8 )

import qualified Data.Text as Text

import Euler.Answers

fastTest :: Test
fastTest = testGroup "Problems with fast answers"
    [ answerTest  1 answer1
    , answerTest  2 answer2
    , answerTest  3 answer3
    , answerTest  5 answer5
    , answerTest  6 answer6
    , answerTest  7 answer7
    , answerTest  8 answer8
    , answerTest  9 answer9
    , answerTest 11 answer11
    , answerTest 13 answer13
    , answerTest 15 answer15
    , answerTest 16 answer16
    , answerTest 17 answer17
    , answerTest 18 answer18
    , answerTest 19 answer19
    , answerTest 20 answer20
    , answerTest 22 answer22
    , answerTest 28 answer28
    , answerTest 29 answer29
    , answerTest 33 answer33
    , answerTest 38 answer38
    , answerTest 40 answer40
    , answerTest 42 answer42
    , answerTest 67 answer67
    ]

answerTest :: Int -> String -> Test
answerTest i x = testCase ("Problem " ++ (show i) ++ " answer is correct")
                          (x @?= answerOf i)

answerOf :: Int -> String
answerOf i = snd $ head $ filter (\(x, _) -> x == s) answers where s = show i

answers :: [(String, String)]
answers = parseAnswers inputString where
    parseAnswers = (map parseLine) . lines
    parseLine line = case words line of [x, y] -> (x, y)

inputString :: String
inputString = Text.unpack $ decodeUtf8 $(embedFile "../answers.txt")
