module EulerTest.Problems
    ( tests
    , answerTest
    , answerTestMain
    ) where

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

import Control.Monad      ( join )

import Data.Bifunctor     ( bimap )
import Data.List          ( find )
import Data.Maybe         ( mapMaybe )
import Data.Text          ( Text )

import qualified Data.Text    as Text
import qualified Data.Text.IO as TextIO

import qualified Euler.Problems

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
answerTestMain i = defaultMainWithArgs [answerTest i] []

-- | @'getCorrectAnswer' n@ reads the known answer for Euler problem /n/
-- from @answers.txt@ at the root of the repository, or 'Nothing' if the
-- file doesn't contain the answer for that problem.
getCorrectAnswer :: Integral a => a -> IO (Maybe String)

-----------------------------------------------------------------------

tests = [ testGroup "Problems with fast answers" $ map answerTest xs ]
  where
    xs :: [Int]
    xs = [ 1, 2, 3, 5, 6, 7, 8, 9, 11, 13, 15, 16, 17, 18, 19, 20, 22
         , 28, 29, 33, 40, 42, 66, 67 ]

answerTest i = testCase name assertion
  where
    name = "Problem " ++ showIntegral i ++ " answer is correct"
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
    answers = mapMaybe parseLine $ Text.lines text
      where
        parseLine :: Text -> Maybe (String, String)
        parseLine line = case Text.words line of
                           [x, y] -> Just $ join bimap Text.unpack (x, y)
                           _      -> Nothing

showIntegral :: Integral a => a -> String
showIntegral i = show ((fromIntegral i) :: Integer)
