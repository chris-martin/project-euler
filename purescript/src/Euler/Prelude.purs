module Euler.Prelude
  ( module Prelude
  , module Control.Monad.Aff
  , module Control.Monad.Eff.Exception
  , module Data.Array
  , module Data.BigInt
  , module Data.Foldable
  , module Data.Int
  , module Data.List.Lazy
  , module Data.List.Unsafe
  , module Data.Maybe
  , module Data.Maybe.Unsafe
  , module Data.Tuple
  , module Math
  , module Node.FS
  ) where

import Prelude
  ( ($), (<$>), (==), (<), (<<<), (*), (+)
  , Unit, unit, mod, pure, show
  )

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)

import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Foldable (any, sum)
import Data.Int (even)
import Data.Int as Int
import Data.List.Lazy as ZList
import Data.List.Unsafe (head)
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Data.Tuple (Tuple(..), fst, snd)

import Math (pow)
