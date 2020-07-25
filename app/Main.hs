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
    timeFolder "sudokus/hardest"
    -- solveOne "sudokus/hardest/hardest1.txt"
    -- solveOne "sudokus/hardest/hardest2.txt" -- quite slow
    -- solveOne "sudokus/hardest/hardest3.txt"
    -- solveOne "sudokus/hardest/hardest4.txt"
    -- solveOne "sudokus/hardest/hardest5.txt"
    -- solveOne "sudokus/hardest/hardest6.txt"
    -- solveOne "sudokus/hardest/hardest7.txt"
    -- solveOne "sudokus/hardest/hardest8.txt"
    -- solveOne "sudokus/hardest/hardest9.txt"
    -- solveOne "sudokus/hardest/hardest10.txt"
    -- solveOne "sudokus/hardest/hardest11.txt"
    -- solveOne "sudokus/hardest/hardest12.txt" -- very slow
    -- solveOne "sudokus/hardest/hardest13.txt"
    -- solveOne "sudokus/hardest/hardest14.txt"
    -- solveOne "sudokus/hardest/hardest15.txt"
    -- solveOne "sudokus/hardest/hardest16.txt"
    -- solveOne "sudokus/hardest/hardest17.txt"
    -- solveOne "sudokus/hardest/hardest18.txt"
    -- solveOne "sudokus/hardest/hardest19.txt"
    -- solveOne "sudokus/hardest/hardest20.txt"
    -- solveOne "sudokus/hardest/hardest21.txt"
    -- solveOne "sudokus/hardest/hardest22.txt"
    -- solveOne "sudokus/hardest/hardest23.txt"
    -- solveOne "sudokus/hardest/hardest24.txt"
    -- solveOne "sudokus/hardest/hardest25.txt"
    -- solvePairs "sudokus/sudoku.csv"
    -- solveFolder "sudokus/hardest"