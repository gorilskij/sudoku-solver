{-# LANGUAGE TupleSections #-}

module Search 
-- (sudokuDFS) 
where

import LibBase
import BitSet
import Data.List (find)
import Data.Foldable (asum)
import Control.Applicative (Alternative)

import Debug.Trace

type Assumption = (Index, Int)

cell :: ICell -> [Assumption]
cell c@(_, Full _) = []
cell (i, Empty as) = map (i,) $ toList as

-- associate by fst and replace value when match found
replaceInByFst :: Eq i => (i, a) -> [(i, a)] -> [(i, a)]
replaceInByFst x@(i, _) (y@(j, _):ys)
    | i == j    = x : ys
    | otherwise = y : replaceInByFst x ys

row :: Group -> [(Assumption, Group)]
row (id, g) = map (\a@(i, v) -> (a, (id, replaceInByFst (i, Full v) g))) as -- incomprehensible
    where as = concat $ map cell g

possibleNextSteps :: Sudoku -> [(Assumption, Sudoku)]
possibleNextSteps rs = map (\(a, g) -> (a, replaceInByFst g rs)) gs
    where gs = concat $ map row rs

orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing = id
orElse m       = const m


removeBadAssumptions'' :: [Assumption] -> ICell -> ICell
removeBadAssumptions'' [] c      = c
removeBadAssumptions'' _ c@(_, Full _)     = c
removeBadAssumptions'' ((i, v):as) c@(j, Empty vs)
    | i == j    = removeBadAssumptions'' as (i, Empty (v #< vs))
    | otherwise = removeBadAssumptions'' as c

removeBadAssumptions' :: [Assumption] -> Group -> Group
removeBadAssumptions' = mapSnd . map . removeBadAssumptions''

removeBadAssumptions :: [Assumption] -> Sudoku -> Sudoku
removeBadAssumptions = map . removeBadAssumptions'


sudokuDFS :: (Sudoku -> MaybeFeasible Sudoku) -- deterministic solving step
          -> Sudoku                           -- start sudoku
          -> MaybeFeasible Sudoku             -- winning element if feasible
sudokuDFS solveDet sudoku
    | isSolved sudoku = pure sudoku
    | otherwise       = asum (map (sudokuDFS solveDet) goodSudoku')
    where
          nextSteps = map (mapSnd solveDet) $ possibleNextSteps sudoku
          -- acc -> acc -> ...
          splitBadAssumptions :: [Assumption] -> [Sudoku] -> [(Assumption, MaybeFeasible Sudoku)] -> ([Assumption], [Sudoku])
          splitBadAssumptions as ss []               = (as, ss)
          splitBadAssumptions as ss ((a, maybeS):xs) =
              case maybeS of
                  Infeasible -> splitBadAssumptions (a : as) ss xs
                  Feasible s -> splitBadAssumptions as (s : ss) xs
          (badAssumptions, goodSudoku) = splitBadAssumptions [] [] nextSteps
          goodSudoku' = map (removeBadAssumptions badAssumptions) goodSudoku
