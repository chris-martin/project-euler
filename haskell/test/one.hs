{-

Checks the answer for a particular problem.

-}

{-# LANGUAGE LambdaCase #-}

import Euler.Test (answerTest)

import System.Environment (getArgs)
import Test.Framework

main :: IO ()
main =
  getArgs >>=
  \case
    n:args -> defaultMainWithArgs [answerTest (read n :: Integer)] args
    _      -> mempty
