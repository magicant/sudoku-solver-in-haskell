module Board where

import Data.Array
import Data.List


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


type Board a = Array Index a

boardOfList :: [a] -> Board a
boardOfList = listArray indexBounds

-- | Create a board with all cells filled with the argument.
board :: a -> Board a
board c = boardOfList $ repeat c

mapBoard :: (a -> b) -> Board a -> Board b
mapBoard f b = boardOfList $ map f $ elems b

possibilityBoard :: Board Int -> Board [Int]
possibilityBoard = mapBoard possibility
  where possibility n | inRange numBounds n = [n]
                      | otherwise           = range numBounds

unboxPossibilityBoard :: Board [Int] -> Board Int
unboxPossibilityBoard = mapBoard unbox
  where unbox [n] = n

-- | If the cell at the argument index has exactly one possibility,
-- return the possibility. Otherwise, return a null list.
fixedValue :: Board [a] -> Index -> [a]
fixedValue b i =
  case b ! i of
    poss @ [c] -> poss
    _          -> []

-- | Collect all fixed values in the argument indexes.
fixedValues :: Board [a] -> [Index] -> [a]
fixedValues b = concatMap (fixedValue b)

-- | Indexes where the same number cannot occur.
excludedIndexes :: Index -> [Index]
excludedIndexes i = delete i (irow `union` icol `union` iblock)
  where
    irow   = range $ rowBounds $ row i
    icol   = range $ colBounds $ col i
    iblock = range $ blockBounds i

-- | Numbers that cannot occur at the argument index.
impossibilities :: Board [a] -> Index -> [a]
impossibilities b i = fixedValues b $ excludedIndexes i

eliminateImpossibilities :: Board [Int] -> Board [Int]
eliminateImpossibilities b =
  boardOfList [(b ! i) \\ impossibilities b i | i <- range indexBounds]

fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f a = fst $ head $ dropWhile (uncurry (/=)) $ zip as $ tail as
  where as = iterate f a

-- | Repeat simulation to find all possible solutions.
simulate :: Board [Int] -> [Board [Int]]
simulate b = simulate' (range indexBounds)
  where
    simulate' [] = [b']
    simulate' (i:is) =
      case b' ! i of
      [] -> []
      [n] -> simulate' is
      ns -> concatMap (\n -> simulate (b' // [(i, [n])])) ns
    b' = fixedPoint eliminateImpossibilities b

-- | All possible soltions.
solve :: Board Int -> [Board Int]
solve = map unboxPossibilityBoard . simulate . possibilityBoard

sampleBoard :: Board Int
sampleBoard = listArray indexBounds [
  1, 0, 0, 4, 0, 0, 7, 0, 9,
  0, 5, 0, 7, 8, 0, 0, 2, 0,
  7, 0, 9, 0, 2, 3, 0, 0, 6,
  3, 0, 0, 6, 0, 0, 0, 0, 0,
  6, 4, 0, 0, 7, 0, 0, 1, 2,
  9, 0, 8, 0, 0, 2, 0, 4, 5,
  2, 3, 0, 5, 0, 4, 8, 0, 0,
  0, 6, 0, 0, 9, 0, 0, 3, 0,
  8, 0, 7, 0, 0, 1, 0, 6, 4]
{-
  1, 2, 3, 4, 5, 6, 7, 8, 9,
  4, 5, 6, 7, 8, 9, 1, 2, 3,
  7, 8, 9, 1, 2, 3, 4, 5, 6,
  3, 1, 2, 6, 4, 5, 9, 7, 8,
  6, 4, 5, 9, 7, 8, 3, 1, 2,
  9, 7, 8, 3, 1, 2, 6, 4, 5,
  2, 3, 1, 5, 6, 4, 8, 9, 7,
  5, 6, 4, 8, 9, 7, 2, 3, 1,
  8, 9, 7, 2, 3, 1, 5, 6, 4]
-}

unsolvableBoard :: Board Int
unsolvableBoard = board 0 // [((1,1),1), ((1,2),1)]

-- vim: et sw=2 sts=2