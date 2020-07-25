-- this is to avoid cyclic imports, idk how else

module LibBase where

import BitSet
import Data.WideWord (Int128)
import Data.List (transpose)
import Data.List.Split (chunksOf)

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
