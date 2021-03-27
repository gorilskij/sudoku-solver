-- this is to avoid cyclic imports, idk how else

module LibBase where

import BitSet

import Data.WideWord (Int128)
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (mapMaybe)
import Control.Applicative (Alternative, empty, (<|>))

type Value = Int
type Index = Int
type Allowed = BitSet Int -- stores values 1-9
-- this might be faster as a list
type Indices = BitSet Int128 -- stores values 0-80

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

-- for debug purposes
instance Show Cell where
    -- same as showCell' in Visual
    show (Full n)   = "%    " ++ show n ++ "    %"
    show (Empty as) = "["
                       ++ map (\n -> if n `member` as
                                         then head $ show n
                                         else ' ') [1..9]
                       ++ "]"

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

isFull :: ICell -> Bool
isFull (_, Full _) = True
isFull _           = False

isEmpty = not . isFull

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
toGroups f = zip (map f [0..])

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


isSolved :: Sudoku -> Bool
isSolved = all (all isFull . snd)


data MaybeFeasible a = Feasible a
                     | Infeasible
                     deriving (Eq, Show)

instance Functor MaybeFeasible where
    fmap f (Feasible x) = Feasible (f x)
    fmap _ _            = Infeasible

instance Applicative MaybeFeasible where
    pure x = Feasible x
    (Feasible f) <*> (Feasible x) = Feasible (f x)
    _ <*> _                       = Infeasible

instance Alternative MaybeFeasible where
    empty = Infeasible
    Infeasible <|> x = x
    f <|> _          = f

instance Monad MaybeFeasible where
    (Feasible x) >>= f = f x
    _ >>= _            = Infeasible

-- isFeasible :: Sudoku -> Bool
-- isFeasible s = all isFeasible' s
--              && all isFeasible' (rows2cols s)
--              && all isFeasible' (rows2squares s)
--     where isFeasible' g
--               | hasDuplicates g = False
--               | 

--           hasDuplicates' :: BitSet Int -> [Value] -> Bool
--           hasDuplicates' bs (v:vs)
--               | v `member` bs = True
--               | otherwise     = hasDuplicates' (v #> bs) vs
--           hasDuplicates = hasDuplicates' empty
--                         . map snd
--                         . mapMaybe unwrapFull
--                         . snd