module Board.Parse (parse) where

import Data.Array
import Data.Char
import Board

digitToNum :: Char -> Int
digitToNum '1' = 1
digitToNum '2' = 2
digitToNum '3' = 3
digitToNum '4' = 4
digitToNum '5' = 5
digitToNum '6' = 6
digitToNum '7' = 7
digitToNum '8' = 8
digitToNum '9' = 9
digitToNum _   = 0

parseCells :: Int -> [Char] -> [(Index, Int)]
parseCells row cs = zip indexes cells
  where
    indexes = range $ rowBounds row
    cells = map digitToNum cs

parseLines :: [String] -> Board Int
parseLines lines = array indexBounds cells
  where
    cells = do
      (lineNo, line) <- zip [1..9] lines
      parseCells lineNo line

parse :: String -> (Board Int, String)
parse s = (parseLines boardLines, unlines remainder)
  where (boardLines, remainder) = splitAt 9 $ lines s

-- vim: et sw=2 sts=2
