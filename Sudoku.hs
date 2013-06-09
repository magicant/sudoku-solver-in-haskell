module Main where

import Board
import Board.Parse
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
main = getContents >>= (printSolutions . solve . fst . parse)

-- vim: et sw=2 sts=2
