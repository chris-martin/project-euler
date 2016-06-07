module Test.Main where

import Euler.Problems (answer)

import Prelude (Unit, unit, ($), (<$>), (==), (<<<), (>>>),
                (++), bind, pure, show, return)
import Prim (Array, Int, String)

import Control.Bind ((=<<))

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)

import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Array (head, filter)
import Data.Array.WordsLines (lines, words)
import Data.Foldable (sequence_)
import Data.Functor (($>))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.String as String

import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff (readTextFile)

import Test.Unit (Test, TestSuite, suite, test, success, failure)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)

main = runTest $ suite "Problems with fast answers" $ sequence_ $ answerTest <$> [1, 2]

-- | @'answerTest' n@ is a test case that checks the answer for Euler
-- problem /n/ against the known answers in @answers.txt@ at the root
-- of the repository.
answerTest :: forall e. Int -> TestSuite (err :: EXCEPTION, fs :: FS, testOutput :: TESTOUTPUT | e)
answerTest i = test name assertion
  where
  name = "Problem " ++ show i ++ " answer is correct"
  assertion :: Test (err :: EXCEPTION, fs :: FS, testOutput :: TESTOUTPUT | e)
  assertion = do
    correctAnswerMaybe <- getCorrectAnswer i
    case correctAnswerMaybe of
      Nothing -> failure "The correct answer is not known"
      Just correctAnswer -> do
        case answer i of
          Nothing -> failure "We don't have a solution"
          Just calculatedAnswerAff -> do
            calculatedAnswer <- calculatedAnswerAff
            Assert.equal correctAnswer calculatedAnswer

-- | @'getCorrectAnswer' n@ reads the known answer for Euler problem /n/
-- | from @answers.txt@ at the root of the repository, or 'Nothing' if the
-- | file doesn't contain the answer for that problem.
getCorrectAnswer :: forall e. Int -> Aff (err :: EXCEPTION, fs :: FS | e) (Maybe String)
getCorrectAnswer i = do
  text <- readTextFile UTF8 "../answers.txt"
  return $ parseCorrectAnswer i text

parseCorrectAnswer :: Int -> String -> Maybe String
parseCorrectAnswer i text = snd <$> find (((==) (show i)) <<< fst) answers
  where
  answers :: Array (Tuple String String)
  answers = parseLine <$> lines text
    where
    parseLine :: String -> Tuple String String
    parseLine line = case words line of [x, y] -> Tuple x y
  find f = filter f >>> head
