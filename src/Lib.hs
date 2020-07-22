-- module Lib
--        ( Sudoku
--        , fromString
--        , disallow, onlyChoice, iterateToStability
--        ) where
module Lib where

import BitSet9
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Function
import Control.Monad
import Data.Char
import Debug.Trace

type Value = Int
type Index = Int
type Allowed = BitSet9

data Cell = Full Value
          | Empty Allowed
          deriving Eq

type ICell = (Index, Cell) -- indexed cell
type Group = [ICell]
type Sudoku = [Group]

showCell' :: ICell -> String
showCell' (_, Full n)   = "%    " ++ show n ++ "    %"
showCell' (i, Empty as) = "["
                        ++ map (\n -> if n `member` as
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
             $ map (map readCell . rPad9 ' ')
             $ rPad9 ""
             $ lines s
    where rPad9 x = take 9 . (++ (repeat x))
          readCell ' ' = Empty full
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
disallow' g = map (mapEmpty (\\\ disallowed)) g
    where disallowed :: Allowed
          disallowed = fromList $ map snd $ mapMaybe unwrapFull g

disallow :: Sudoku -> Sudoku
disallow = squares2rows
         . map disallow' -- on squares
         . rows2squares
         . cols2rows
         . map disallow' -- on cols
         . rows2cols
         . map disallow' -- on rows

data CliqueType = Type1 | Type2 | Type3 deriving (Show, Eq)
type CliqueData = ([Index], Allowed) -- (ids, values)
data Clique = Clique CliqueType CliqueData deriving Show

safeInit [] = []
safeInit l  = init l

-- whole group -> subsequence -> ...
tryIntoClique :: [(Index, Allowed)] -> [(Index, Allowed)] -> Maybe Clique
tryIntoClique g ias = let (is, as) = unzip ias
                          
                          (a':as') = as
                          isClique1 = length is == len a' && all (a' ==) as'

                          sharedAs = intersections as
                          otherCells = filter ( not
                                              . (`elem` is)
                                              . fst
                                              ) g
                          otherAs = unions $ map snd otherCells
                          uniqueAs = sharedAs \\\ otherAs
                      in if isClique1
                             then Just $ Clique Type1 (is, a')
                             else case len uniqueAs `compare` length is of
                                      LT -> if len uniqueAs == 0
                                                then Nothing -- a lot of these
                                                else Just $ Clique Type3 (is, uniqueAs)
                                      EQ -> Just $ Clique Type2 (is, uniqueAs)
                                      GT -> error "invalid sudoku"

cliques' :: Group -> [Clique]
cliques' g = mapMaybe (tryIntoClique g')
         $ safeInit
         $ tail
         $ subsequences
         $ g'
    where g' = mapMaybe unwrapEmpty g

cliques :: Sudoku -> [Clique]
cliques s -- maybe nub here somehow
        = rowCs ++ colCs ++ squareCs
    where sCs = foldl1 (++) . map cliques'
          rowCs = sCs s
          colCs = sCs $ rows2cols s
          squareCs = sCs $ rows2squares s

-- this function takes up 40% of execution time and 50% of allocation space
-- check if group contains clique
contains :: Group -> Clique -> Bool
contains g (Clique _ (is, _)) = is `isSubsequenceOf` (map fst g)
-- contains g (Clique _ (is, _)) = fromNubList is `isSubSetOf` fromList (map fst g)

applyAll :: [a -> a] -> a -> a
applyAll [] x     = x
applyAll (f:fs) x = applyAll fs (f x)

-- disallow clique values from all other group members
-- assumes (g `contains` c1)
disallowCliqueA :: CliqueData -> Group -> Group
disallowCliqueA (is, vs) g = map removeVsIfOther g
    where removeVsIfOther (i, Empty as)
              | not $ i `elem` is = (i, Empty (as \\\ vs))
          removeVsIfOther c = c

-- disallow all non-clique values from clique members
-- assumes (g `contains` c2)
disallowCliqueB :: CliqueData -> Group -> Group
disallowCliqueB (is, vs) g = map removeAsIfMember g
    where removeAsIfMember (i, Empty as)
              | i `elem` is = (i, Empty vs)
          removeAsIfMember c = c

disallowClique' :: Clique -> Group -> Group
disallowClique' c@(Clique Type1 cd) = disallowCliqueA cd
disallowClique' c@(Clique Type2 cd) = disallowCliqueA cd
                                    . disallowCliqueB cd
disallowClique' c@(Clique Type3 cd) = disallowCliqueA cd

disallowCliques' :: [Clique] -> Group -> Group
disallowCliques' cs g = applyAll (map disallowClique' myCs) g
    where myCs = filter (g `contains`) cs

disallowCliques :: Sudoku -> Sudoku
disallowCliques s = if length cs == 0
                        then s
                        else sub -- on rows
                           $ cols2rows
                           $ sub -- on cols
                           $ rows2cols
                           $ squares2rows
                           $ sub -- on squares
                           $ rows2squares
                           $ s
    where cs = cliques s
          sub = map $ disallowCliques' cs

-- convert empty cell with one allowed value into full cells
onlyChoice :: Sudoku -> Sudoku
onlyChoice = map (map onlyChoice')
    where onlyChoice' (i, Empty as)
              | len as == 1 = (i, Full $ head $ toList as)
          onlyChoice' c     = c


solvingRound :: Sudoku -> Sudoku
solvingRound = onlyChoice . disallowCliques . disallow

solve :: Sudoku -> Sudoku
solve s = let iters = iterate solvingRound s
          in fst $ head $ dropWhile (uncurry (/=)) $ zip iters $ tail iters
