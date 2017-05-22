{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module Euler.Test
  ( getCorrectAnswer
  , parseCorrectAnswer
  , answerTest
  ) where

import Euler.Prelude

import qualified Euler.Problems

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

-- | @'getCorrectAnswer' n@ reads the known answer for Euler problem /n/
-- from @answers.txt@ at the root of the repository, or 'Nothing' if the
-- file doesn't contain the answer for that problem.
getCorrectAnswer :: Integer -> IO (Maybe String)
getCorrectAnswer i =
  TextIO.readFile "../answers.txt" <&> parseCorrectAnswer i

parseCorrectAnswer :: Integer -> Text -> Maybe String
parseCorrectAnswer i text =
    snd <$> find ((== showInteger i) . fst) answers
  where
    answers :: [(String, String)]
    answers = mapMaybe parseLine $ Text.lines text

    parseLine :: Text -> Maybe (String, String)
    parseLine line =
      case Text.words line of
        [x, y] -> Just $ join bimap Text.unpack (x, y)
        _      -> Nothing

-- | @'answerTest' n@ is a test case that checks the answer for Euler
-- problem /n/ against the known answers in @answers.txt@ at the root
-- of the repository.
answerTest :: Integer -> Test
answerTest i =
    testCase ("Problem " <> showInteger i <> " answer is correct") assertion
  where
    assertion :: IO ()
    assertion =
      Map.lookup i Euler.Problems.answers &
      \case
        Nothing -> assertFailure "no solution is given"
        Just (solutionIO :: IO String) ->
          getCorrectAnswer i >>=
          \case
            Nothing -> assertFailure "answer is not known"
            Just correctAnswer ->
              solutionIO >>= \(solution :: String) ->
              solution @?= correctAnswer
