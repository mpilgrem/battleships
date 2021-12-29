{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}

module Battleships where

import Data.List (foldl')
import Data.Maybe (isNothing)

import Data.Array.IArray (Array, Ix, array)
import qualified Data.Array.IArray as A
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

data CellState = Sea | Boat
  deriving (Eq, Show)

data Orient = H | V
  deriving (Eq, Ix, Ord, Show)

type Fleet = [Int]

type Position = (Int, Int)

type Direction = (Int, Int)

type RunOrigin = (Orient, Position)

data Board = Board
  { width  :: Int
  , height :: Int
  , cells  :: Array (Int, Int) (Maybe CellState)
  , info   :: Array Orient (Vector Int)
  } deriving (Eq, Show)

ortho :: Orient -> Orient
ortho H = V
ortho V = H

index :: Orient -> Position -> Int
index H (i, _) = i
index V (_, j) = j

origin :: Orient -> Position -> Position
origin H (_, j) = (0, j)
origin V (i, _) = (i, 0)

location :: Orient -> Int -> Int -> Position
location H j i = (i, j)
location V i j = (i, j)

dim :: Orient -> Board -> Int
dim H b = width b
dim V b = height b

count :: (a -> Bool) -> [a] -> Int
count p = go 0
 where go !n [] = n
       go !n (x:xs) | p x       = go (n + 1) xs
                    | otherwise = go n xs

emptyBoard :: Int -> Int -> Board
emptyBoard w h = Board
  { width  = w
  , height = h
  , cells  = array ((0, 0), (w - 1, h - 1))
                   [ ((i, j), Nothing) | i <- [0 .. w - 1]
                                       , j <- [0 .. h - 1] ]
  , info   = mkInfo (V.replicate h 0) (V.replicate w 0)
  }

rowToCells :: Int -> String -> [((Int, Int), Maybe CellState)]
rowToCells r row = snd $ foldl' process (0, []) row
 where
  process (n, a) c = case c of
    ' ' -> (n + 1, a)
    '@' -> (n + 1, ((n, r), Just Boat) : a)
    '#' -> (n + 1, ((n, r), Just Sea) : a)
    _   -> undefined

toCells :: [String] -> [((Int, Int), Maybe CellState)]
toCells rows = snd $ foldl' process (0, []) rows
 where
  process (n, a) row = (n + 1, rowToCells n row <> a)

printBoard :: Board -> IO ()
printBoard = putStr . showBoard

showBoard :: Board -> String
showBoard b =
  concatMap showRow [0 .. height b - 1] <>
  showVInfo
 where
  showRow r =
    map (\c -> showCell (cells b A.! (c, r))) [0 .. width b - 1] <>
    " " <>
    show ((info b A.! H) V.! r) <>
    "\n"
  showCell mCellStatus = case mCellStatus of
    Nothing -> ' '
    Just Sea -> '\x2592'
    Just Boat -> '\x2588'
  showVInfo =
    concatMap show (V.toList $ info b A.! V) <>
    "\n"

printFleet :: Fleet -> IO ()
printFleet = putStr . showFleet

showFleet :: Fleet -> String
showFleet f = unwords (map (`replicate` '\x2588') f) <> "\n"

mkInfo :: Vector Int -> Vector Int -> Array Orient (Vector Int)
mkInfo hInfo vInfo = array (H, V) [ (H, hInfo), (V, vInfo) ]

valid :: Orient -> Board -> Int -> Position -> Bool
valid o b s p
  | index o p + s > dim o b = False
  | s > limit' b o (index (ortho o) p) = False
  | countBoat' b (o, p) s + s > limit b (o, p) = False
  | otherwise = isRun o b p s

limit :: Board -> RunOrigin -> Int
limit b (o, p) = limit' b o (index (ortho o) p)

limit' :: Board -> Orient -> Int -> Int
limit' b o i = info b A.! o V.! i

countBoat :: Board -> Orient -> Int -> Int
countBoat b o i = count test [0 .. dim o b - 1]
 where
  test j = cells b A.! (location o i j) == Just Boat

countBoat' :: Board -> RunOrigin -> Int -> Int
countBoat' b (o, p) s = count test (pre <> post)
 where
  test p' = cells b A.! p' == Just Boat
  pre = run o (origin o p) (index o p)
  postLen = dim o b - index o p - s
  post = run o (p `add` scale s f) postLen
  (_, _, _, f) = dirs o

isNotBoat :: Board -> Position -> Bool
isNotBoat b (i, j)
  | i < 0 || j < 0 || i >= width b || j >= height b = True
  | otherwise = cells b A.! (i, j) /= Just Boat

isNotSea :: Board -> Position -> Bool
isNotSea b (i, j) = cells b A.! (i, j) /= Just Sea

isBlank :: Board -> Position -> Bool
isBlank b (i, j) = isNothing $ cells b A.! (i, j)

isRun :: Orient -> Board -> Position -> Int -> Bool
isRun o b p s = any (isBlank b) run' &&
                all (isNotBoat b) (neighbours o p s) &&
                isValidRun (ortho o) b run'
 where
  run' = run o p s

isValidRun :: Orient -> Board -> [Position] -> Bool
isValidRun o b = all validCell
 where
  validCell p = case cells b A.! p of
    Just Sea  -> False
    Just Boat -> True
    Nothing   -> let idx = index (ortho o) p
                 in  countBoat b o idx < limit' b o idx

run :: Orient -> Position -> Int -> [Position]
run o p s = map (\m -> p `add` scale m f) [0 .. s - 1]
 where
  (_, _, _, f) = dirs o

validNeighbours :: Board -> Orient -> Position -> Int -> [Position]
validNeighbours b o p s = filter onBoard (neighbours o p s)
 where
  onBoard (i, j) = i >= 0 && j >= 0 && i < width b && j < height b

neighbours :: Orient -> Position -> Int -> [Position]
neighbours o p s = (p `add` b) : (p `add` scale s f) : foldl' lrs [] [-1, 0 .. s]
 where
  (l, r, b, f) = dirs o

  lrs :: [Position] -> Int -> [Position]
  lrs a m = let p' = p `add` scale m f
            in  (p' `add` l) : (p' `add` r) : a

dirs :: Orient -> (Direction, Direction, Direction, Direction)
dirs H = ((0, -1), (0, 1), (-1, 0), (1, 0))
dirs V = ((-1, 0), (1, 0), (0, -1), (0, 1))

add :: Position -> Direction -> Position
add (x, y) (dx, dy) = (x + dx, y + dy)

scale :: Int -> Direction -> Direction
scale m (dx, dy) = (m * dx, m * dy)

allRuns :: Board -> Int -> [RunOrigin]
allRuns b s = runs H <> runs V
 where
  runs o = map (o,) $ filter (valid o b s) (A.indices $ cells b)

isComplete :: Board -> Bool
isComplete b = all (isComplete' H) [0 .. height b - 1] &&
               all (isComplete' V) [0 .. width b - 1]
 where
  isComplete' o i = countBoat b o i == limit' b o i

solve :: Board -> Fleet -> Maybe Board
solve b [] = if isComplete b then Just b else Nothing
solve b (s:ss) = let rs = allRuns b s
                     bs = map (place b s) rs
                 in  if null rs then Nothing else solve' bs ss

solve' :: [Board] -> Fleet -> Maybe Board
solve' [] _ = error "No boards!"
solve' [b] f = solve b f
solve' (b:bs) f = maybe (solve' bs f) Just (solve b f)

place :: Board -> Int -> RunOrigin -> Board
place b s (o, p) =
  let cs = cells b A.// (boats <> seas)
      boats = map (,Just Boat) (run o p s)
      seas = map (,Just Sea) (validNeighbours b o p s)
  in  b {cells = cs}
