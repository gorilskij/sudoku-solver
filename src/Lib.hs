{-# LANGUAGE UnboxedSums #-}

module Lib ( Sudoku
           , fromString
           , fromFile
           , pairsFromCSV
           , showSudoku
           , showSudoku'
           , isSolved
           , solve
           ) where

import LibBase
import Visual
import Clique

import BitSet
import Data.Char (digitToInt, isDigit)
import Data.List (intercalate)
import Data.List.Split (splitOn, chunksOf)
import Data.Maybe (mapMaybe)

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll _ []    = []
removeAll rs (x:xs)
    | x `elem` rs =     removeAll rs xs
    | otherwise   = x : removeAll rs xs

replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ []  = []
replace x y (z:zs)
    | x == z    = y : replace x y zs
    | otherwise = z : replace x y zs

-- from dense 9x9 representation where spaces are empty cells
-- preprocessing: all '|' and '-' are removed, '.' becomes ' '
fromString :: String -> Sudoku
fromString s = map (\(i, g) -> (Row i, zip [i * 9 ..] g))
             $ zip [0..]
             $ map (map readCell . rPad9 ' ')
             $ rPad9 ""
            --  $ (\x -> trace ("row" ++ show x) x)
             $ filter (not . (0 ==) . length)
             $ lines
             $ replace '.' ' '
             $ removeAll ['|', '-']
             $ s
    where rPad9 x = take 9 . (++ (repeat x))
          readCell ' ' = Empty $ fromList [1..9]
          readCell c   = Full $ digitToInt c

fromFile :: FilePath -> IO Sudoku
fromFile = (fromString <$>) . readFile

-- with 0s representing empty cells and no newlines
-- ... -> [(Puzzle, Solution)]
pairsFromCSV :: FilePath -> IO [(Sudoku, Sudoku)]
pairsFromCSV = ( map toPair
             . splitOn "\n"
             . dropWhile (not . isDigit)
             <$>)
             . readFile
    where toPair = (\[a, b] -> (parse a, parse b))
                 . splitOn ","
          parse = fromString . changeFormat
          changeFormat = intercalate "\n" . chunksOf 9 . replace '0' ' '
          replace _ _ [] = []
          replace x y (z:zs)
              | x == z    = y : replace x y zs
              | otherwise = z : replace x y zs


mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

disallow' :: Group -> Group
disallow' g = mapSnd (map (mapEmpty (\\\ disallowed))) g
    where disallowed = fromList
                     $ map snd
                     $ mapMaybe unwrapFull
                     $ snd g

disallow :: Sudoku -> Sudoku
disallow = squares2rows
         . map disallow' -- on squares
         . rows2squares
         . cols2rows
         . map disallow' -- on cols
         . rows2cols
         . map disallow' -- on rows

-- convert empty cell with one allowed value into full cells
onlyChoice :: Sudoku -> Sudoku
onlyChoice = map $ mapSnd $ map onlyChoice'
    where onlyChoice' :: ICell -> ICell
          onlyChoice' (i, Empty as)
              | len as == 1 = (i, Full $ head $ toList as)
          onlyChoice' c     = c


solvingRound :: Sudoku -> Sudoku
solvingRound = onlyChoice . disallowCliques . disallow

-- solve deterministically, no search
solveDet :: Sudoku -> Sudoku
solveDet s = let iters = iterate solvingRound s
             in fst $ head $ dropWhile (uncurry (/=)) $ zip iters $ tail iters

solve :: Sudoku -> Sudoku
solve = solveDet