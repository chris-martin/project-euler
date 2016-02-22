{-# LANGUAGE OverloadedStrings #-}

module EulerTest.Answers
    ( fastTest
    , answerTest
    ) where

import Test.HUnit ((@?=))
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Data.Text          ( Text )
import Data.Text.Encoding ( decodeUtf8 )

import qualified Data.Text    as Text
import qualified Data.Text.IO as TextIO

import qualified Euler.Answers

fastTest :: Test
fastTest = testGroup "Problems with fast answers" $
    map answerTest [ 1, 2, 3, 5, 6, 7, 8, 9, 11, 13, 15, 16
                   , 17, 18, 19, 20, 22, 28, 29, 33, 38, 40
                   , 42, 67 ]

answerTest :: Int -> Test
answerTest i = testCase ("Problem " ++ (show i) ++ " answer is correct") $ do
    correctAnswer <- getCorrectAnswer i
    calculatedAnswer <- Euler.Answers.answer i
    calculatedAnswer @?= correctAnswer

getCorrectAnswer :: Int -> IO String
getCorrectAnswer i = do
  text <- TextIO.readFile "../answers.txt"
  let answers = map parseLine $ Text.lines text where
      parseLine :: Text -> (String, String)
      parseLine line = case Text.words line of [x, y] -> (Text.unpack x, Text.unpack y)
  return $ snd $ head $ filter (\(x, _) -> x == s) answers where s = show i
