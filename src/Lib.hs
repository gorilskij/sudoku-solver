-- module Lib
--        ( Sudoku
--        , fromString
--        , disallow, onlyChoice, iterateToStability
--        ) where
module Lib where

import qualified Data.Set as Set
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Function
import Control.Monad
import Data.Char
import Debug.Trace

type Value = Int
type Index = Int
type Allowed = Set.Set Value

data Cell = Full Value
          | Empty Allowed
          deriving Eq

type ICell = (Index, Cell) -- indexed cell
type Group = [ICell]
type Sudoku = [Group]

showCell' :: ICell -> String
showCell' (_, Full n)   = "%    " ++ show n ++ "    %"
showCell' (i, Empty as) = "["
                        ++ map (\n -> if n `Set.member` as
                                          then head $ show n
                                          else ' ') [1..9]
                        ++ "]"

showGroup' :: Group -> String
showGroup' = intercalate " | "
           . map (intercalate " ")
           . chunksOf 3
           . map showCell'

-- debug show
showSudoku' :: Sudoku -> String
showSudoku' = intercalate ("\n"
                           ++ intercalate "+" (replicate 3 (replicate 37 '-'))
                           ++ "\n")
            . map (intercalate "\n")
            . chunksOf 3
            . map (' ' :)
            . map showGroup'

showCell :: ICell -> String
showCell (_, Full n)  = show n
showCell (_, Empty _) = " "

showGroup :: Group -> String
showGroup = intercalate " | "
          . map (intercalate " ")
          . chunksOf 3
          . map showCell

-- pretty show
showSudoku :: Sudoku -> String
showSudoku = intercalate ("\n"
                          ++ intercalate "+" (replicate 3 (replicate 7 '-'))
                          ++ "\n")
           . map (intercalate "\n")
           . chunksOf 3
           . map (' ' :)
           . map showGroup

-- from dense 9x9 representation where spaces are empty cells
fromString :: String -> Sudoku
fromString s = map (\(s, g) -> zip [s..] g)
             $ zip [0,9..]
             $ map (map readCell . assert9 . rPad9)
             $ lines s
    where rPad9 = take 9 . (++ (repeat ' '))
          assert9 l
              | length l == 9 = l
              | otherwise     = error $ "length of \"" ++ l ++ "\" is not 9"
          readCell ' ' = Empty $ Set.fromList [1..9]
          readCell c = Full $ read [c]

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

isFull :: ICell -> Bool
isFull (_, Full _) = True
isFull _           = False

isEmpty = not . isFull

isSolved :: Sudoku -> Bool
isSolved = all $ all isFull

unwrapFull :: ICell -> Maybe (Index, Value)
unwrapFull (i, Full n) = Just (i, n)
unwrapFull _           = Nothing

unwrapEmpty :: ICell -> Maybe (Index, Allowed)
unwrapEmpty (i, Empty as) = Just (i, as)
unwrapEmpty _             = Nothing

mapEmpty :: (Allowed -> Allowed) -> ICell -> ICell
mapEmpty g (i, Empty as) = (i, Empty (g as))
mapEmpty _ f             = f

-- the default representation of Sudoku is rows
rows2cols :: Sudoku -> Sudoku
rows2cols = transpose

cols2rows :: Sudoku -> Sudoku
cols2rows = transpose

rows2squares :: Sudoku -> Sudoku
rows2squares = concatMap ( map (concat . transpose)
                         . chunksOf 3
                         . transpose
                         )
             . chunksOf 3

squares2rows :: Sudoku -> Sudoku
squares2rows = concatMap ( map concat
                         . transpose
                         . map (chunksOf 3)
                         )
             . chunksOf 3

disallow' :: Group -> Group
disallow' g = map (mapEmpty (Set.\\ disallowed)) g
    where disallowed :: Allowed
          disallowed = foldl (flip Set.insert)
                             Set.empty
                             ( map snd
                             $ mapMaybe unwrapFull g
                             )

disallow :: Sudoku -> Sudoku
disallow = squares2rows
         . map disallow' -- on squares
         . rows2squares
         . cols2rows
         . map disallow' -- on cols
         . rows2cols
         . map disallow' -- on rows

type Clique = ([Index], Allowed) -- (ids, values)

-- then Just else Nothing
(?>) :: Bool -> a -> Maybe a
True  ?> a = Just a
False ?> _ = Nothing

tryIntoClique1 :: [(Index, Allowed)] -> Maybe Clique
tryIntoClique1 ias = isClique1 ?> (is, a)
    where (is, (a:as)) = unzip ias
          isClique1 = length is == length a && all (a ==) as

safeInit [] = []
safeInit l  = init l

-- same as Set.unions (error on empty list)
intersections :: Ord a => [Set.Set a] -> Set.Set a
intersections = foldl1 Set.intersection

-- whole group -> subsequence -> (maybe clique1, maybe clique2)
tryIntoClique23 :: [(Index, Allowed)] -> [(Index, Allowed)] -> (Maybe Clique, Maybe Clique)
tryIntoClique23 g ias = let (is, as) = unzip ias;
                            sharedAs = intersections as;
                            otherCells = filter ( not
                                                . (`elem` is)
                                                . fst
                                                ) g
                            otherAs = Set.unions $ map snd otherCells
                            uniqueAs = sharedAs Set.\\ otherAs
                        in case length uniqueAs `compare` length is of
                               LT -> if length uniqueAs == 0
                                         then (Nothing, Nothing) -- a lot of these
                                         else (Nothing, Just (is, uniqueAs))
                               EQ -> (Just (is, uniqueAs), Nothing)
                               GT -> error "invalid sudoku"

type Cliques123 = ([Clique], [Clique], [Clique])

cliques' :: Group -> Cliques123
cliques' g = ( mapMaybe tryIntoClique1 subSeqs
             , catMaybes maybe2s
             , catMaybes maybe3s
             )
    where g' = mapMaybe unwrapEmpty g
          subSeqs = safeInit
                  $ tail
                  $ subsequences
                  $ g'
          (maybe2s, maybe3s) = unzip $ map (tryIntoClique23 g') subSeqs

cliques :: Sudoku -> Cliques123
cliques = foldl1 (+++)
        . map cliques'
    where (a, b, c) +++ (d, e, f) = (a ++ d, b ++ e, c ++ f)

-- this function takes up 40% of execution time and 50% of allocation space
-- check if group contains clique
contains :: Group -> Clique -> Bool
-- contains g (is, _) = Set.fromList is `Set.isSubsetOf` Set.fromList (map fst g)
contains g (is, _) = is `isSubsequenceOf` sort (map fst g)

applyAll :: [a -> a] -> a -> a
applyAll [] x     = x
applyAll (f:fs) x = applyAll fs (f x)

-- disallow clique values from all other group members
-- assumes (g `contains` c1)
disallowClique1' :: Clique -> Group -> Group
disallowClique1' c1@(is, vs) g = map removeVsIfOther g
    where removeVsIfOther (i, Empty as)
              | not $ i `elem` is = (i, Empty (as Set.\\ vs))
          removeVsIfOther c = c

-- disallow all non-clique values from clique members
-- assumes (g `contains` c2)
disallowClique2' :: Clique -> Group -> Group
disallowClique2' c2@(is, vs) g = map removeAsIfMember g
    where removeAsIfMember (i, Empty as)
              | i `elem` is = (i, Empty vs)
          removeAsIfMember c = c

disallowCliques'' :: ([Clique], [Clique], [Clique]) -> Group -> Group
disallowCliques'' (c1s, c2s, c3s) g = applyAll ( map disallowClique1' c1s'
                                               ++ map disallowClique1' c2s'
                                               ++ map disallowClique1' c3s'
                                               ++ map disallowClique2' c2s'
                                               ) g
    where flt = filter (g `contains`)
          c1s' = flt c1s
          c2s' = flt c2s
          c3s' = flt c3s

disallowCliques' :: ([Clique], [Clique], [Clique]) -> Sudoku -> Sudoku
disallowCliques' = map . disallowCliques''

disallowCliques :: Sudoku -> Sudoku
disallowCliques s = sub -- on rows
                  $ cols2rows
                  $ sub -- on cols
                  $ rows2cols
                  $ squares2rows
                  $ sub -- on squares
                  $ rows2squares
                  $ s
    where sub = disallowCliques' $ cliques s

-- convert empty cell with one allowed value into full cells
onlyChoice :: Sudoku -> Sudoku
onlyChoice = map (map onlyChoice')
    where onlyChoice' (i, Empty as)
              | length as == 1 = (i, Full $ head $ Set.toList as)
          onlyChoice' c = c


solvingRound :: Sudoku -> Sudoku
solvingRound = onlyChoice . disallowCliques . disallow

solve :: Sudoku -> Sudoku
solve s = let iters = iterate solvingRound s
          in fst $ head $ dropWhile (uncurry (/=)) $ zip iters $ tail iters
