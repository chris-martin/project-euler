module EulerTest.Problems
    ( tests
    , answerTest
    , answerTestMain
    ) where

import EulerTest.Prelude

import Data.List          ( find )
import Data.Text          ( Text )
import Data.Text.Encoding ( decodeUtf8 )

import qualified Data.Text    as Text
import qualified Data.Text.IO as TextIO

import qualified Euler.Problems

import qualified EulerTest.Problems.Problem9
import qualified EulerTest.Problems.Problem43
import qualified EulerTest.Problems.Problem46

-----------------------------------------------------------------------

-- | The test suite for Euler problems. It includes checks for answers
-- for problems that have suitably fast answers, and tests for problems'
-- modules.
tests :: [Test]

-- | @'answerTest' n@ is a test case that checks the answer for Euler
-- problem /n/ against the known answers in @answers.txt@ at the root
-- of the repository.
answerTest :: Integral a => a -> Test

-- | Used for standalone test suites that check answers for slow problems.
answerTestMain :: Integer -> IO ()
answerTestMain i = defaultMain [answerTest i]

-- | @'getCorrectAnswer' n@ reads the known answer for Euler problem /n/
-- from @answers.txt@ at the root of the repository, or 'Nothing' if the
-- file doesn't contain the answer for that problem.
getCorrectAnswer :: Integral a => a -> IO (Maybe String)

-----------------------------------------------------------------------

tests = concat
  [ [fastAnswerTests]
  , EulerTest.Problems.Problem9.tests
  , EulerTest.Problems.Problem43.tests
  , EulerTest.Problems.Problem46.tests
  ]
  where
    fastAnswerTests = testGroup "Problems with fast answers" $ map answerTest
      [ 1, 2, 3, 5, 6, 7, 8, 9, 11, 13, 15, 16, 17, 18, 19, 20, 22, 28, 29
      , 33, 40, 42, 67 ]

answerTest i = testCase name assertion
  where
    name = "Problem " ++ (show (fromIntegral i)) ++ " answer is correct"
    assertion = do
      correctAnswerMaybe <- getCorrectAnswer i
      case correctAnswerMaybe of
        Nothing -> assertFailure "answer is not known"
        Just correctAnswer -> do
          calculatedAnswer <- Euler.Problems.answer i
          calculatedAnswer @?= correctAnswer

getCorrectAnswer i = do
  text <- TextIO.readFile "../answers.txt"
  return (parseCorrectAnswer i text)

parseCorrectAnswer :: Integral a => a -> Text -> Maybe String
parseCorrectAnswer i text = fmap snd (find ((== showIntegral i) . fst) answers)
  where
    answers :: [(String, String)]
    answers = map parseLine $ Text.lines text
      where
        parseLine :: Text -> (String, String)
        parseLine line = case Text.words line of
                           [x, y] -> (Text.unpack x, Text.unpack y)

showIntegral :: Integral a => a -> String
showIntegral i = show ((fromIntegral i) :: Integer)
