module Euler.Problem.Problem16 (answer) where

answer :: Integer
answer = sum $ digits $ 2 ^ 1000
    where digits = (map charToInt) . show
          charToInt c = read [c]
