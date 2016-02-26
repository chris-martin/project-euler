-- | This isn't a very generally-useful module, but it's included in util
-- because it's needed by both Euler problems 18 and 67 (which are essentially
-- the same problem).
module Euler.Util.TrianglePath
    (
    -- * Types
      Triangle
    , Row

    -- * Functions
    , parseTriangle
    , reduceTriangle
    ) where

import Data.Maybe         ( catMaybes )
import Data.List.NonEmpty ( NonEmpty(..), nonEmpty )
import Data.Text          ( Text )

import qualified Data.List.NonEmpty as NE
import qualified Data.Text          as T

import Euler.Util.List (neTails)

type Row      = NonEmpty Int
type Triangle = NonEmpty Row

parseTriangle :: Text -> Triangle
parseTriangle = NE.fromList . reverse . catMaybes . (map parseRow) . T.lines
    where parseRow = nonEmpty . (map (read . T.unpack)) . T.words

reduceTriangle :: Triangle -> Int

-- If there's one row, choose its maximum element.
reduceTriangle (row :| []) = maximum row

-- If there are two rows and the second has one element, add it
-- with the larger of the first two elements of the first row.
reduceTriangle (row :| [x :| []]) = x + (maximum $ NE.take 2 row)

-- Otherwise, collapse the first two rows by reducing small triangles
-- formed by zipping the second row with suffixes of the first row.
reduceTriangle (row1 :| row2 : otherRows) =
    reduceTriangle (newFirstRow :| otherRows)
    where newFirstRow = do (row, x) <- NE.zip (neTails row1) row2
                           return (reduceTriangle (row :| [x :| []]))
