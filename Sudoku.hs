module Main where

import Board
import Board.Pretty

printSolutions :: Show a => [Board a] -> IO ()
printSolutions bs = printSolutions' $ zip bs [1..]
  where
    printSolutions' [] = return ()
    printSolutions' ((b,n):bs) =
      do
        putStrLn ("Solution " ++ show n ++ ":")
        putStrLn (pretty b)
        printSolutions' bs

main :: IO ()
main = printSolutions (solve sampleBoard)
-- FIXME: Solve arbitrary boards

-- vim: et sw=2 sts=2
