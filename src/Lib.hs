-- module Lib
--        ( Sudoku
--        , fromString
--        , disallow, onlyChoice, iterateToStability
--        ) where
module Lib where

import BitSet
import Data.WideWord (Int128)
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Function
import Control.Monad
import Data.Char
import Debug.Trace
import Combinatorics (partitions)

type Value = Int
type Index = Int
type Allowed = BitSet Int -- stores values 1-9
-- this might be faster as a list
type Indices = BitSet Int128 -- stores values 0-80 (Int128 might be faster)

data Cell = Full Value
          | Empty Allowed
          deriving Eq

data GroupId = Row Int
             | Col Int
             | Square Int
             deriving (Eq, Show)

type ICell = (Index, Cell) -- indexed cell
type Group = (GroupId, [ICell])
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
           . snd

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
          . snd

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
fromString s = map (\(i, g) -> (Row i, zip [i * 9 ..] g))
             $ zip [0..]
             $ map (map readCell . rPad9 ' ')
             $ rPad9 ""
             $ lines s
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

isFull :: ICell -> Bool
isFull (_, Full _) = True
isFull _           = False

isEmpty = not . isFull

isSolved :: Sudoku -> Bool
isSolved = all (all isFull . snd)

unwrapFull :: ICell -> Maybe (Index, Value)
unwrapFull (i, Full n) = Just (i, n)
unwrapFull _           = Nothing

unwrapEmpty :: ICell -> Maybe (Index, Allowed)
unwrapEmpty (i, Empty as) = Just (i, as)
unwrapEmpty _             = Nothing

mapEmpty :: (Allowed -> Allowed) -> ICell -> ICell
mapEmpty g (i, Empty as) = (i, Empty (g as))
mapEmpty _ f             = f

toGroups :: (Int -> GroupId) -> [[ICell]] -> Sudoku
toGroups = toGroups' 0
    where toGroups' i f (g:gs) = (f i, g) : toGroups' (i + 1) f gs
          toGroups' _ _ []     = []

fromGroups :: Sudoku -> [[ICell]]
fromGroups = map snd

-- the default representation of Sudoku is rows
rows2cols :: Sudoku -> Sudoku
rows2cols = toGroups Col
          . transpose
          . fromGroups

cols2rows :: Sudoku -> Sudoku
cols2rows = toGroups Row
          . transpose
          . fromGroups

rows2squares :: Sudoku -> Sudoku
rows2squares = toGroups Square
             . concatMap ( map (concat . transpose)
                         . chunksOf 3
                         . transpose
                         )
             . chunksOf 3
             . fromGroups

squares2rows :: Sudoku -> Sudoku
squares2rows = toGroups Row
             . concatMap ( map concat
                         . transpose
                         . map (chunksOf 3)
                         )
             . chunksOf 3
             . fromGroups

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

data CliqueType = Type1 | Type2 | Type3 deriving (Show, Eq)
type CliqueData = (Indices, Allowed) -- (ids, values)
data Clique = Clique { cliqueType :: CliqueType
                     , creationGroup :: GroupId
                     , containingGroups :: [GroupId]
                     , cliqueData :: CliqueData
                     } deriving Show

safeInit [] = []
safeInit l  = init l

rowIndices :: [(GroupId, Indices)]
rowIndices = [ (Row 0, fromList [00..08])
             , (Row 1, fromList [09..17])
             , (Row 2, fromList [18..26])
             , (Row 3, fromList [27..35])
             , (Row 4, fromList [36..44])
             , (Row 5, fromList [45..53])
             , (Row 6, fromList [54..62])
             , (Row 7, fromList [63..71])
             , (Row 8, fromList [72..80])
             ]

colIndices :: [(GroupId, Indices)]
colIndices = [ (Col 0, fromList [0,09..72])
             , (Col 1, fromList [1,10..73])
             , (Col 2, fromList [2,11..74])
             , (Col 3, fromList [3,12..75])
             , (Col 4, fromList [4,13..76])
             , (Col 5, fromList [5,14..77])
             , (Col 6, fromList [6,15..78])
             , (Col 7, fromList [7,16..79])
             , (Col 8, fromList [8,17..80])
             ]

squareIndices :: [(GroupId, Indices)]
squareIndices = [ (Square 0, fromList [00,01,02, 09,10,11, 18,19,20])
                , (Square 1, fromList [03,04,05, 12,13,14, 21,22,23])
                , (Square 2, fromList [06,07,08, 15,16,17, 24,25,26])
                
                , (Square 3, fromList [27,28,29, 36,37,38, 45,46,47])
                , (Square 4, fromList [30,31,32, 39,40,41, 48,49,50])
                , (Square 5, fromList [33,34,35, 42,43,44, 51,52,53])

                , (Square 6, fromList [54,55,56, 63,64,65, 72,73,74])
                , (Square 7, fromList [57,58,59, 66,67,68, 75,76,77])
                , (Square 8, fromList [60,61,62, 69,70,71, 78,79,80])
                ]

allEqual :: Eq a => [a] -> Bool
allEqual (a:a':as)
    | a == a'   = allEqual $ a' : as
    | otherwise = False
allEqual as     = True

-- type (rows, cols, squares) to ignore -> set of indices -> ...
containingGroupsExcept :: GroupId -> Indices -> [GroupId]
containingGroupsExcept g is = find $ setsExcept g
    where find = map fst . take 1 . filter ((is `isSubsetOf`) . snd)
          setsExcept :: GroupId -> [(GroupId, Indices)]
          setsExcept (Row _) = colIndices ++ squareIndices
          setsExcept (Col _) = rowIndices ++ squareIndices
          setsExcept (Square _) = rowIndices ++ colIndices

type BareEmptyICell = (Index, Allowed)

-- _ -> (other group members, potential clique) -> ...
tryIntoClique :: GroupId -> ([BareEmptyICell], [BareEmptyICell]) -> Maybe Clique
tryIntoClique gId (otherIAs, ias) = let (is, as) = unzip ias
                                        is' = fromList is
                                        cGs = containingGroupsExcept gId is'

                                        (a':as') = as
                                        isClique1 = length is == len a' && all (a' ==) as'
     
                                        sharedAs = intersections as
                                        otherAs = unions $ map snd otherIAs
                                        uniqueAs = sharedAs \\\ otherAs
                                    in if isClique1
                                           then Just $ Clique Type1 gId cGs (is', a')
                                           else case len uniqueAs `compare` length is of
                                                    LT -> if len uniqueAs == 0
                                                              then Nothing -- a lot of these
                                                              else Just $ Clique Type3 gId cGs (is', uniqueAs)
                                                    EQ -> Just $ Clique Type2 gId cGs (is', uniqueAs)
                                                    GT -> error $ "invalid sudoku"

cliques' :: Group -> [Clique]
cliques' (gId, g) = mapMaybe (tryIntoClique gId)
                  $ safeInit -- do we even need single element cliques?
                --   $ tail
                  $ dropWhile ((< 2) . length . snd) -- without single element cliques
                  $ partitions -- [(others, subsequence)]
                  $ mapMaybe unwrapEmpty
                  $ g

cliques :: Sudoku -> [Clique]
cliques s -- maybe nub here somehow
        = rowCs ++ colCs ++ squareCs
    where sCs = foldl' (++) [] . map cliques'
          rowCs = sCs s
          colCs = sCs $ rows2cols s
          squareCs = sCs $ rows2squares s

-- this function takes up 40% of execution time and 50% of allocation space
-- check if group contains clique
contains :: Group -> Clique -> Bool
contains (gId, _) Clique{ creationGroup=cId
                        , containingGroups=containingGroups
                        }
    = gId == cId
    || gId `elem` containingGroups
-- contains g (Clique _ (is, _)) = fromNubList is `isSubSetOf` fromList (map fst g)

applyAll :: [a -> a] -> a -> a
applyAll [] x     = x
applyAll (f:fs) x = applyAll fs (f x)

-- disallow clique values from all other group members
-- assumes (g `contains` c1)
disallowCliqueA :: CliqueData -> [ICell] -> [ICell]
disallowCliqueA (is, vs) g = map removeVsIfOther g
    where removeVsIfOther (i, Empty as)
              | not $ i `member` is = (i, Empty (as \\\ vs))
          removeVsIfOther c = c

-- disallow all non-clique values from clique members
-- assumes (g `contains` c2)
disallowCliqueB :: CliqueData -> [ICell] -> [ICell]
disallowCliqueB (is, vs) g = map removeAsIfMember g
    where removeAsIfMember (i, Empty as)
              | i `member` is = (i, Empty vs)
          removeAsIfMember c = c


-- do we need disallow at all?
disallowClique' :: Clique -> Group -> Group
disallowClique' Clique{cliqueType=Type1, cliqueData=cd} (gId, g) = (gId, disallowCliqueA cd g)

disallowClique' Clique{cliqueType=Type2, creationGroup=cId, cliqueData=cd} gr@(gId, g)
    | cId == gId = (gId, disallowCliqueB cd g)
    | otherwise  = (gId, disallowCliqueA cd $ disallowCliqueB cd g)

disallowClique' Clique{cliqueType=Type3, creationGroup=cId, cliqueData=cd} gr@(gId, g)
    | cId == gId = gr
    | otherwise  = (gId, disallowCliqueA cd g)


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
onlyChoice = map $ mapSnd $ map onlyChoice'
    where onlyChoice' :: ICell -> ICell
          onlyChoice' (i, Empty as)
              | len as == 1 = (i, Full $ head $ toList as)
          onlyChoice' c     = c


solvingRound :: Sudoku -> Sudoku
solvingRound = onlyChoice . disallowCliques . disallow

solve :: Sudoku -> Sudoku
solve s = let iters = iterate solvingRound s
          in fst $ head $ dropWhile (uncurry (/=)) $ zip iters $ tail iters
