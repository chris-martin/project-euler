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
                       , Test.DocTest.doctest ["src"]
                       ]
        [n] -> EulerTest.Problems.answerTestMain ((read n) :: Integer)

reifyExitCode :: IO () -> IO ExitCode
reifyExitCode io = (\x -> case x of
    Left code -> code
    _ -> ExitSuccess) <$> try io

exitMax :: [IO ()] -> IO ()
exitMax = (>>= (exitWith . foldr' max ExitSuccess)) . sequence . (map reifyExitCode)
