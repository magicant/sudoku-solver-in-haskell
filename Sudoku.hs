module Main where

import Board

main :: IO ()
main = print resultBoard
  where resultBoard = solve sampleBoard
-- FIXME: Solve arbitrary boards
-- FIXME: Pretty printing

-- vim: et sw=2 sts=2
