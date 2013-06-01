module Board where

import Data.Array


type Index = (Int, Int)

row :: Index -> Int
row = fst

col :: Index -> Int
col = snd

numBounds :: (Int, Int)
numBounds = (1, 9)

rowBounds :: Int -> (Index, Index)
rowBounds r = ((r, fst numBounds), (r, snd numBounds))

colBounds :: Int -> (Index, Index)
colBounds c = ((fst numBounds, c), (snd numBounds, c))

-- | Bounds of the block containing the argument index.
blockBounds :: Index -> (Index, Index)
blockBounds (r, c) = ((rl, cl), (ru, cu))
  where (rl, ru) = bounds r
        (cl, cu) = bounds c
        bounds n | 1 <= n && n <= 3 = (1, 3)
                 | 4 <= n && n <= 6 = (4, 6)
                 | 7 <= n && n <= 9 = (7, 9)

indexBounds :: (Index, Index)
indexBounds = ((l, l), (u, u))
  where l = fst numBounds; u = snd numBounds

-- vim: et sw=2 sts=2
