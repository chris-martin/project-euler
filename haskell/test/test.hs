module Main (main) where

import qualified EulerTest.Problems

import qualified Test.DocTest
import qualified System.Environment
import System.Exit (ExitCode(..), exitWith)
import Control.Exception (try)
import Data.Foldable (foldr')

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

main :: IO ()
main = do
    args <- System.Environment.getArgs
    case args of
        []  -> exitMax [ defaultMainWithArgs EulerTest.Problems.tests []
                       , Test.DocTest.doctest ["src"] ]
        [n] -> EulerTest.Problems.answerTestMain ((read n) :: Integer)

exitMax :: [IO ()] -> IO ()
exitMax = (>>= (exitWith . maxExitCode)) . sequence . (map reifyExitCode)
  where reifyExitCode = (fmap $ either id $ const ExitSuccess) . try
        maxExitCode = foldr' max ExitSuccess
