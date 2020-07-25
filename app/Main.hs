module Main where

import Lib

import Debug.Trace
import System.Directory
import System.FilePath.Posix
import Data.Function
import Data.Char
import Data.List


--
solveOne :: FilePath -> IO ()
solveOne p = do
    s <- fromFile p
    putStrLn $ showSudoku s
    putStrLn "\n\n\n"

    putStrLn $ showSudoku' s
    putStrLn "\n\n\n"

    let s' = solve s

    putStrLn $ showSudoku' s'
    putStrLn "\n\n\n"

    putStrLn $ showSudoku s'
--


--
solvePairs''' i pss
    | i `mod` 100 == 0 = trace ("iteration " ++ show i) (solvePairs'' i pss)
    | otherwise        = solvePairs'' i pss

solvePairs'' :: Int -> [(Sudoku, Sudoku)] -> IO ()
solvePairs'' _ [] = return ()
solvePairs'' i ((p, s):pss) = do
    let solved = solve p
    if isSolved solved
        then if solved == s
                 then solvePairs''' (i + 1) pss
                 else error $ "solved but not equal at index " ++ show i
        else error $ "failed to solve at index " ++ show i

solvePairs' = solvePairs'' 0

solvePairs :: String -> IO ()
solvePairs f = do
    pairs <- pairsFromCSV f
    solvePairs' $ take 1000
                $ pairs
--


--
(.:) = (.) . (.)

endsWith :: String -> String -> Bool
endsWith = on (and .: zipWith (==)) reverse

fileNum :: String -> Int
fileNum = read
        . takeWhile isDigit
        . dropWhile (not . isDigit)

solveFolder :: FilePath -> IO ()
solveFolder f = do
    subs <- getDirectoryContents f
    let subs' = filter (endsWith ".txt") subs
    let subs'' = sortOn fileNum subs'
    let subs''' = map (f </>) subs''
    print subs'''
    putStrLn "\n\n"

    sequence
     $ intersperse (putStrLn "\n\n\n===========================================\n\n\n")
     $ map solveOne subs'''

    return ()
--

main :: IO ()
main = do
    -- solveOne "sudokus/test4.txt"
    solvePairs "sudokus/sudoku.csv"
    -- solveFolder "sudokus/hardest"