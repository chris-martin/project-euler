{-

Checks the answer for a particular problem.

-}

{-# LANGUAGE LambdaCase #-}

import Euler.Test (answerTest)

import Control.Monad ((>>=))
import Data.Monoid (mempty)
import Prelude (Integer, read)
import System.Environment (getArgs)
import System.IO (IO)
import Test.Framework

main :: IO ()
main =
  getArgs >>=
  \case
    n:args -> defaultMainWithArgs [answerTest (read n :: Integer)] args
    _      -> mempty
