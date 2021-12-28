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
valid o b s (i, j)
  | o == H
  , i + s > width b = False
  | o == H
  , s > info b A.! H V.! j = False
  | o == V
  , j + s > height b = False
  | o == V
  , s > info b A.! V V.! i = False
  | countBoat' b (o, (i, j)) s + s > limit b (o, (i, j)) = False
  | otherwise = isRun o b (i, j) s

limit :: Board -> RunOrigin -> Int
limit b (o, (i, j)) = info b A.! o V.! rc
 where
  rc = case o of
         H -> j
         V -> i

countBoat :: Orient -> Board -> Int -> Int
countBoat o b i = let count n j = if cells b A.! p == Just Boat
                                    then n + 1
                                    else n
                       where
                        p = case o of
                              H -> (j, i)
                              V -> (i, j)
                  in  foldl' count 0 [0 .. l - 1]

 where
  l = case o of
        H -> width b
        V -> height b

countBoat' :: Board -> RunOrigin -> Int -> Int
countBoat' b (o, p@(i, j)) s = let count n p' = if cells b A.! p' == Just Boat
                                                  then n + 1
                                                  else n
                               in  foldl' count 0 (pre <> post)
 where
  origin = case o of
             H -> (0, j)
             V -> (i, 0)
  preLen = case o of
             H -> i
             V -> j
  pre = run o origin preLen
  postLen = case o of
              H -> width b - i - s
              V -> height b  - j - s
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
  validCell p@(i, j) = case cells b A.! p of
    Just Sea  -> False
    Just Boat -> True
    Nothing   -> countBoat o b rc < info b A.! o V.! rc
   where
    rc = case o of
           H -> j
           V -> i

run :: Orient -> Position -> Int -> [Position]
run o p s = map (\m -> p `add` scale m f) [0 .. s - 1]
 where
  (_, _, _, f) = dirs o

validNeighbours :: Board -> Orient -> Position -> Int -> [Position]
validNeighbours b o (i, j) s = filter onBoard (neighbours o (i, j) s)
 where
  onBoard (i', j') = i' >= 0 && j' >= 0 && i' < width b && j' < height b

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

runs :: Orient -> Board -> Int -> [Position]
runs o b s = filter (valid o b s) (A.indices $ cells b)

allRuns :: Board -> Int -> [RunOrigin]
allRuns b s = map (H,) (runs H b s) <> map (V,) (runs V b s)

isComplete :: Board -> Bool
isComplete b = all (isComplete' H b) [0 .. height b - 1] &&
               all (isComplete' V b) [0 .. width b - 1]

isComplete' :: Orient -> Board -> Int -> Bool
isComplete' o b i = countBoat o b i == info b A.! o V.! i

solve :: Board -> Fleet -> Maybe Board
solve b f
  | null f = if isComplete b
               then Just b
               else Nothing
  | otherwise = let s = head f
                    rs = allRuns b s
                in  if null rs
                      then Nothing
                      else let bs = map (\r -> place b r s) rs
                           in  solve' bs (tail f)

solve' :: [Board] -> Fleet -> Maybe Board
solve' [] _ = error "No boards!"
solve' [b] f = solve b f
solve' (b:bs) f =
  case solve b f of
    Nothing -> solve' bs f
    Just b' -> Just b'

place :: Board -> RunOrigin -> Int -> Board
place b (o, p) s =
  let cs = cells b
      cs' = cs A.// (boats <> seas)
      boats = map (,Just Boat) (run o p s)
      seas = map (,Just Sea) (validNeighbours b o p s)
  in  b {cells = cs'}
