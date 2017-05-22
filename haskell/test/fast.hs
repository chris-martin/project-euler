{-

Checks the answers for problems that have reasonably fast solutions.

-}

import Test.Framework

import Euler.Test (answerTest)

main :: IO ()
main = defaultMain
  [ testGroup "Problems with fast answers" $ answerTest <$>
      [ 1, 2, 3, 5, 6, 7, 8, 9, 11, 13, 15, 16, 17, 18, 19, 20, 22
      , 28, 29, 33, 40, 42, 66, 67, 68 ]
  ]
