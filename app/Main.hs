{-# LANGUAGE TupleSections #-}
module Main where

import qualified Data.Array.IArray as A
import qualified Data.Vector.Unboxed as V

import Battleships

testBoard1 :: Board
testBoard1 = let b = emptyBoard 10 10
            in  b { cells = cells b A.// testCells
                  , info  = mkInfo hInfo vInfo
                  }
 where
  hInfo = V.fromList [2, 2, 1, 2, 2, 1, 4, 1, 3, 2]
  vInfo = V.fromList [4, 2, 1, 2, 2, 4, 1, 2, 1, 1]
  testCells = toCells [ "#######@@#"
                      , "    ######"
                      , "#####@####"
                      , "    #@# # "
                      , "    # # # "
                      , "        # "
                      , "        # "
                      , "###     # "
                      , "@@#     # "
                      , "###     # " ]

testBoard2 :: Board
testBoard2 = let b = emptyBoard 3 3
            in  b { cells = cells b A.// testCells
                  , info  = mkInfo hInfo vInfo
                  }
 where
  hInfo = V.fromList [3, 0, 2]
  vInfo = V.fromList [2, 2, 1]
  testCells = toCells [ "   "
                      , "   "
                      , "   " ]

testFleet1 :: Fleet
testFleet1 = [4, 3, 3, 2, 1, 1, 1, 1]

testFleet2 :: Fleet
testFleet2 = [3, 2]

main :: IO ()
main = do
  printBoard testBoard1
  printFleet testFleet1
  putStr "\n"
  let mB = solve testBoard1 testFleet1
  case mB of
    Nothing -> putStrLn "No solution found."
    Just b -> printBoard b
