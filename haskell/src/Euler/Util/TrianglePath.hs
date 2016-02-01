module Euler.Util.TrianglePath
    ( parseTriangle
    , reduceTriangle
    ) where

import Data.Maybe         ( catMaybes )
import Data.List.NonEmpty ( NonEmpty(..), nonEmpty )
import Data.Text          ( Text )
import qualified Data.List.NonEmpty as NE
import qualified Data.Text          as T

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
    reduceTriangle $ newFirstRow :| otherRows
    where newFirstRow = do (row, x) <- NE.zip (neTails row1) row2
                           return $ reduceTriangle $ row :| [x :| []]

-- Like 'tails', but only the non-empty tails from a non-empty list.
-- The result is non-empty because every non-empty list has at least
-- one non-empy suffix (itself). For example,
--
-- > neTails (NE.fromList "abc") == NE.fromList [ NE.fromList "abc"
--                                              , NE.fromList "bc"
--                                              , NE.fromList "c"]

-- todo - could do this more simply
neTails :: NonEmpty a -> NonEmpty (NonEmpty a)
neTails = NE.fromList . catMaybes . NE.toList . (fmap NE.nonEmpty) . NE.tails
