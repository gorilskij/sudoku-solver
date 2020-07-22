module Main where

import Lib

import Debug.Trace

solvem'' i pss
    | i `mod` 100 == 0 = trace ("iteration " ++ show i) (solvem' i pss)
    | otherwise          = solvem' i pss

solvem' :: Int -> [(Sudoku, Sudoku)] -> IO ()
solvem' _ [] = return ()
solvem' i ((p, s):pss) = do
    let solved = solve p
    if isSolved solved
        then if solved == s
                 then solvem'' (i + 1) pss
                 else error $ "solved but not equal at index " ++ show i
        else error $ "failed to solve at index " ++ show i

solvem = solvem' 0


main :: IO ()
main = do
    pairs <- pairsFromCSV "sudokus/sudoku.csv"
    solvem $ take 500 pairs