{-# LANGUAGE TemplateHaskell #-}

module Problem67 (answer) where

import Data.Maybe         ( catMaybes )
import Data.FileEmbed     ( embedFile )
import Data.List.NonEmpty ( NonEmpty(..), nonEmpty )
import Data.Text          ( Text )
import Data.Text.Encoding ( decodeUtf8 )
import qualified Data.List.NonEmpty as NE
import qualified Data.Text          as T

type Row      = NonEmpty Int
type Triangle = NonEmpty Row

answer :: Int
answer = reduceT $ parseT $ text

text :: Text
text = decodeUtf8 $(embedFile "src/triangle.txt")

parseT :: Text -> Triangle
parseT = NE.fromList . reverse . catMaybes . (map parseRow) . T.lines
    where parseRow = nonEmpty . (map (read . T.unpack)) . T.words

reduceT :: Triangle -> Int
-- If there's one row, choose its maximum element.
reduceT (row :| []) = maximum row
-- If there are two rows and the second has one element, add it
-- with the larger of the first two elements of the first row.
reduceT (row :| [x :| []]) = x + (maximum $ NE.take 2 row)
-- Otherwise, collapse the first two rows by reducing small triangles
-- formed by zipping the second row with suffixes of the first row.
reduceT (row1 :| row2 : otherRows) =
    reduceT $ newFirstRow :| otherRows
    where newFirstRow = do (row, x) <- NE.zip (neTails row1) row2
                           return $ reduceT $ row :| [x :| []]

-- Like 'tails', but only the non-empty tails from a non-empty list.
-- The result is non-empty because every non-empty list has at least
-- one non-empy suffix (itself). For example,
--
-- > neTails (NE.fromList "abc") == NE.fromList [ NE.fromList "abc"
--                                              , NE.fromList "bc"
--                                              , NE.fromList "c"]
neTails :: NonEmpty a -> NonEmpty (NonEmpty a)
neTails = NE.fromList . catMaybes . NE.toList . (fmap NE.nonEmpty) . NE.tails
