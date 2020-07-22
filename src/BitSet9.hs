module BitSet9 ( BitSet9()
               , empty
               , isEmpty
               , full
               , len
               , fromList
               , fromNubList
               , toList
               , member
               , (#>)
               , (#<)
               , (\/)
               , (/\)
               , unions
               , intersections
               , (\\\)
            --    , isSubSetOf -- seems to be broken
               ) where

import Data.Bits
import Data.List

-- only works on 9 elements! (0-9) (relevant for \\\)
newtype BitSet9 = BitSet9 Int deriving Eq

instance Show BitSet9 where
    show bs = "{" ++ (intercalate "," $ map show $ toList bs) ++ "}"

empty :: BitSet9
empty = BitSet9 0

isEmpty :: BitSet9 -> Bool
isEmpty (BitSet9 bs) = bs == 0

full :: BitSet9
full = fromList [1..9]

len :: BitSet9 -> Int
len (BitSet9 bs) = len' bs
    where len' 0 = 0
          len' n
              | 1 .&. n == 1 = 1 + len' (n `div` 2)
              | otherwise    =     len' (n `div` 2)

fullInt = let BitSet9 n = full in n

invertInt :: Int -> Int
invertInt = (fullInt `xor`)

bitAt :: Int -> Int
bitAt = shiftL 1

member :: Int -> BitSet9 -> Bool
member n (BitSet9 bs) = let pos = bitAt n in pos .&. bs == pos

flipBit :: Int -> BitSet9 -> BitSet9
flipBit n (BitSet9 bs) = BitSet9 $ bitAt n `xor` bs

fromList :: [Int] -> BitSet9
fromList = fromNubList . nub

-- assumes input list is nub
fromNubList :: [Int] -> BitSet9
fromNubList = foldl (flip flipBit) empty

toList :: BitSet9 -> [Int]
toList (BitSet9 bs) = ints 0 bs
    where ints _ 0 = []
          ints i n
              | n .&. 1 == 1 = i : ints (i + 1) (n `div` 2)
              | otherwise    =     ints (i + 1) (n `div` 2)

-- insert
infixr 6 #>
(#>) :: Int -> BitSet9 -> BitSet9
n #> bs = if n `member` bs
              then bs
              else flipBit n bs

-- remove
infixr 6 #<
(#<) :: Int -> BitSet9 -> BitSet9
n #< bs = if n `member` bs
              then flipBit n bs
              else bs

-- union
infixl 2 \/
(\/):: BitSet9 -> BitSet9 -> BitSet9
BitSet9 bs1 \/ BitSet9 bs2 = BitSet9 $ bs1 .|. bs2

-- intersection
infixl 3 /\
(/\) :: BitSet9 -> BitSet9 -> BitSet9
BitSet9 bs1 /\ BitSet9 bs2 = BitSet9 $ bs1 .&. bs2

unions :: [BitSet9] -> BitSet9
unions = foldl (\/) empty

intersections :: [BitSet9] -> BitSet9
intersections = foldl1 (/\)

infixl 4 \\\
(\\\) :: BitSet9 -> BitSet9 -> BitSet9
BitSet9 bs1 \\\ BitSet9 bs2 = BitSet9 $ bs1 .&. invertInt bs2

-- infixl 5 `isSubSetOf`
-- isSubSetOf :: BitSet9 -> BitSet9 -> Bool
-- isSubSetOf bs1 bs2 = isEmpty $ bs1 \\\ bs2