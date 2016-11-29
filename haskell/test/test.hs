module Main (main) where

import Euler.Prelude

import qualified EulerTest.Problems

import qualified Test.DocTest
import qualified System.Environment
import System.Exit (ExitCode(..), exitWith)
import Control.Exception (try)

import Test.Framework

main :: IO ()
main = do
    args <- System.Environment.getArgs
    case args of
        []  -> exitMax [ defaultMainWithArgs EulerTest.Problems.tests []
                       , Test.DocTest.doctest ["src"] ]
        [n] -> EulerTest.Problems.answerTestMain (read n :: Integer)
        _   -> undefined

exitMax :: [IO ()] -> IO ()
exitMax = (>>= (exitWith . maxExitCode)) . traverse reifyExitCode
  where
    reifyExitCode = fmap (either id $ const ExitSuccess) . try
    maxExitCode = foldr' max ExitSuccess
