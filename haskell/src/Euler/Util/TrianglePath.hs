{- |

This isn't a very generally-useful module, but it's included in util because
it's needed by both Euler problems 18 and 67 (which are essentially the same
problem).

-}
module Euler.Util.TrianglePath
  (
  -- * Types
    Triangle
  , Row

  -- * Functions
  , parseTriangle
  , reduceTriangle
  ) where

import Euler.Prelude

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import Euler.Util.List (neTails)

import Prelude (read)

import qualified Data.List as List

-----------------------------------------------------------------------

type Row      a = NonEmpty a
type Triangle a = NonEmpty (Row a)

{- |

>>> parseTriangle (T.pack " 1\n2 3")
(2 :| [3]) :| [1 :| []]

-}
parseTriangle :: Integral a => Text -> Triangle a
parseTriangle =
    NE.fromList . List.reverse . mapMaybe parseRow . T.lines
  where
    parseRow = nonEmpty . fmap (readI . T.unpack) . T.words
    readI t = fromIntegral (read t :: Int)

{- |

>>> reduceTriangle (parseTriangle (T.pack " 1\n2 3"))
4

>>> reduceTriangle (parseTriangle (T.pack " 1\n2 3\n7 2 4"))
10

-}
reduceTriangle :: Integral a => Triangle a -> a

-- If there's one row, choose its maximum element.
reduceTriangle (row :| []) =
  List.maximum row

-- If there are two rows and the second has one element, add it
-- with the larger of the first two elements of the first row.
reduceTriangle (row :| [x :| []]) =
  x + List.maximum (NE.take 2 row)

-- Otherwise, collapse the first two rows by reducing small triangles
-- formed by zipping the second row with suffixes of the first row.
reduceTriangle (row1 :| row2 : otherRows) =
    reduceTriangle (newFirstRow :| otherRows)
  where
    newFirstRow = do
        (row, x) <- NE.zip (neTails row1) row2
        return $ reduceTriangle $ row :| [x :| []]
