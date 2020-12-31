module Main where

import Lib

import LibBase -- (MaybeFeasible, Infeasible, Feasible)

import System.Directory
import System.FilePath.Posix
import Data.Function
import Data.Char
import Data.List
import System.CPUTime
import Text.Printf

import Debug.Trace


--
solveOne :: FilePath -> IO ()
solveOne p = do
    s <- fromFile p
    putStrLn $ showSudoku s
    putStrLn "\n\n\n"

    putStrLn $ showSudoku' s
    putStrLn "\n\n\n"

    let s' = solve s

    case s' of
        Infeasible   -> putStrLn "INFEASIBLE!"
        Feasible s'' -> do
                            putStrLn $ showSudoku' s''
                            putStrLn "\n\n\n"
                            putStrLn $ showSudoku s''
--


--
timeOne :: FilePath -> IO ()
timeOne p = do
    putStrLn p
    s <- fromFile p

    start <- getCPUTime
    let s' = solve s
    case s' of
        Infeasible   -> return ()
        Feasible s'' -> putStrLn $ showSudoku s''

    end <- getCPUTime

    let secs = fromIntegral (end - start) / (10 ^ 12) :: Double
    case s' of
        Infeasible   -> putStrLn "INFEASIBLE"
        Feasible s'' -> if isSolved s''
                            then printf "SOLVED in %.3fs" secs
                            else error "IMPOSSIBLE"

timeFolder :: FilePath -> IO ()
timeFolder f = do
    subs <- getDirectoryContents f
    let subs' = filter (endsWith ".txt") subs
    let subs'' = sortOn fileNum subs'
    let subs''' = map (f </>) subs''
    print subs'''
    putStrLn "\n\n"

    start <- getCPUTime
    sequence $ intersperse (putStrLn "\n\n")
             $ map timeOne subs'''
    end <- getCPUTime

    let secs = fromIntegral (end - start) / (10 ^ 12) :: Double
    printf "\n==\nTOTAL in %.3fs" secs

    return ()
--


--
solvePairs''' i pss
    | i `mod` 100 == 0 = trace ("iteration " ++ show i) (solvePairs'' i pss)
    | otherwise        = solvePairs'' i pss

solvePairs'' :: Int -> [(Sudoku, Sudoku)] -> IO ()
solvePairs'' _ [] = return ()
solvePairs'' i ((p, s):pss) = do
    case solve p of
        Infeasible -> error "infeasible in pairs"
        Feasible solved
            | isSolved solved ->
                if solved == s
                    then solvePairs''' (i + 1) pss
                    else error $ "solved but not equal at index " ++ show i
            | otherwise       -> error $ "failed to solve at index " ++ show i

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
    -- solveOne "sudokus/test3.txt"
    timeFolder "sudokus/hardest"
    -- solvePairs "sudokus/sudoku.csv"
    -- solveFolder "sudokus/hardest"
