module Board where

import Control.Monad
import Data.Array
import Data.Function
import Data.List


data Index = Index { row :: Int, col :: Int }
  deriving (Eq, Ord, Ix, Show)

numBounds :: (Int, Int)
numBounds = (1, 9)

blockNumBounds :: [(Int, Int)]
blockNumBounds = [(1, 3), (4, 6), (7, 9)]

rowBounds :: Int -> (Index, Index)
rowBounds r = (Index r (fst numBounds), Index r (snd numBounds))

allRowBounds :: [(Index, Index)]
allRowBounds = map rowBounds (range numBounds)

colBounds :: Int -> (Index, Index)
colBounds c = (Index (fst numBounds) c, Index (snd numBounds) c)

allColBounds :: [(Index, Index)]
allColBounds = map colBounds (range numBounds)

-- | Bounds of the block containing the argument index.
blockBounds :: Index -> (Index, Index)
blockBounds (Index r c) = (Index rl cl, Index ru cu)
  where
    (rl, ru) = boundsContaining r
    (cl, cu) = boundsContaining c
    boundsContaining n = head $ filter (flip inRange n) blockNumBounds

allBlockBounds :: [(Index, Index)]
allBlockBounds =
  do
    (rl, ru) <- blockNumBounds
    (cl, cu) <- blockNumBounds
    return (Index rl cl, Index ru cu)

indexBounds :: (Index, Index)
indexBounds = (Index l l, Index u u)
  where (l, u) = numBounds


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

-- | @possibilities b is@ is a mapping from possible numbers to indexes where
-- the numbers may occur in indexes @is@ on board @b@.
possibilities :: Board [Int] -> [Index] -> Array Int [Index]
possibilities b is = accumArray (flip (:)) [] numBounds assocs
  where assocs = do { i <- is; zip (b ! i) (repeat i) }

-- | Find numbers that can occur at exactly one index within the specified
-- indexes.
uniquePossibilities :: Board [Int] -> [Index] -> [(Int, Index)]
uniquePossibilities b is =
  do
    (n, is') <- assocs $ possibilities b is
    guard $ length is' == 1
    return (n, head is')

-- | Fix numbers that can occur at exactly one index within a line or block
filterUnique :: Board [Int] -> Board [Int]
filterUnique board = board // uniques
  where
    uniques = concatMap uniques' bounds
    uniques' bound = map f $ uniquePossibilities board $ range bound
    f (n, i) = (i, [n])
    bounds = allRowBounds ++ allColBounds ++ allBlockBounds

fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f a =
  let next = f a in
  if next == a then a else fixedPoint f next

-- | Repeat simulation to find all possible solutions.
simulate :: Board [Int] -> [Board [Int]]
simulate b =
  case sortedUnfixedCells of
    []          -> [b'] -- All cells fixed; this is the solution
    ((_, []):_) -> []   -- No number is possible for this cell; unsolvable
    ((i, ns):_) -> concatMap (\n -> simulate (b' // [(i, [n])])) ns
  where
    sortedUnfixedCells = sortBy (compare `on` (length . snd)) unfixedCells
    unfixedCells = filter (\(_, ps) -> length ps /= 1) $ assocs b'
    b' = fixedPoint (filterUnique . eliminateImpossibilities) b

-- | All possible soltions.
solve :: Board Int -> [Board Int]
solve = map unboxPossibilityBoard . simulate . possibilityBoard

-- | A sample board that is easy to solve.
--
-- >>> :{
-- >>> solve sampleBoard == [listArray indexBounds [
-- >>> 1, 2, 3, 4, 5, 6, 7, 8, 9,
-- >>> 4, 5, 6, 7, 8, 9, 1, 2, 3,
-- >>> 7, 8, 9, 1, 2, 3, 4, 5, 6,
-- >>> 3, 1, 2, 6, 4, 5, 9, 7, 8,
-- >>> 6, 4, 5, 9, 7, 8, 3, 1, 2,
-- >>> 9, 7, 8, 3, 1, 2, 6, 4, 5,
-- >>> 2, 3, 1, 5, 6, 4, 8, 9, 7,
-- >>> 5, 6, 4, 8, 9, 7, 2, 3, 1,
-- >>> 8, 9, 7, 2, 3, 1, 5, 6, 4]]
-- >>> :}
-- True
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

-- | A board that has no solution.
--
-- >>> solve unsolvableBoard
-- []
unsolvableBoard :: Board Int
unsolvableBoard = board 0 // [(Index 1 1, 1), (Index 1 2, 1)]

-- vim: et sw=2 sts=2
