module Board.Pretty (pretty) where

import Data.Array
import Board

prettyRow :: Show a => Int -> Board a -> String
prettyRow r b = unwords [show (b ! i) | i <- range $ rowBounds r]

pretty :: Show a => Board a -> String
pretty b = unlines [prettyRow r b | r <- range numBounds]


-- vim: et sw=2 sts=2
