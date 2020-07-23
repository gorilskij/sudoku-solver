module BitSet ( BitSet ()
               , empty
               , isEmpty
               , len
               , fromList
               , toList
               , member
               , (#>)
               , (#<)
               , (\/)
               , (/\)
               , unions
               , intersections
               , (\\\)
               , isSubsetOf -- seems to be broken
               ) where

import Data.Bits
import Data.List

-- could store capacity as the highest bit in the number and do runtime checks...
-- this is not safe if the integral overflows (63 with Int)
newtype BitSet a = BitSet a deriving Eq -- Bits a

instance Functor BitSet where
    fmap f (BitSet bs) = BitSet (f bs)

instance Applicative BitSet where
    pure = BitSet
    BitSet f <*> BitSet x = BitSet (f x)

instance Bits a => Show (BitSet a) where
    show bs = "{" ++ (intercalate "," $ map show $ toList bs) ++ "}"

-- (min, max) -> ...
empty :: Bits a => BitSet a
empty = BitSet zeroBits

isEmpty :: Bits a => BitSet a -> Bool
isEmpty = (empty ==)

len :: Bits a => BitSet a -> Int
len (BitSet bs) = popCount bs

member :: Bits a => Int -> BitSet a -> Bool
member n (BitSet bs) = let pos = bit n in pos .&. bs == pos

fromList :: Bits a => [Int] -> BitSet a
fromList = BitSet
         . foldl setBit zeroBits

toList :: Bits a => BitSet a -> [Int]
toList (BitSet bs) = ints 0 bs
    where ints :: Bits a => Int -> a -> [Int]
          ints i n
              | n == zeroBits        = []
              | n .&. bit 0 == bit 0 = i : ints (i + 1) (shiftR n 1)
              | otherwise            =     ints (i + 1) (shiftR n 1)

-- insert
infixr 6 #>
(#>) :: Bits a => Int -> BitSet a -> BitSet a
(#>) = (<$>) . flip setBit

-- remove
infixr 6 #<
(#<) :: Bits a => Int -> BitSet a -> BitSet a
(#<) = (<$>) . flip clearBit

-- union
infixl 2 \/
(\/) :: Bits a => BitSet a -> BitSet a -> BitSet a
bs1 \/ bs2 = (.|.) <$> bs1 <*> bs2

-- intersection
infixl 3 /\
(/\) :: Bits a => BitSet a -> BitSet a -> BitSet a
bs1 /\ bs2 = (.&.) <$> bs1 <*> bs2

unions :: Bits a => [BitSet a] -> BitSet a
unions = foldl (\/) empty

intersections :: Bits a => [BitSet a] -> BitSet a
intersections = foldl1 (/\)

allBits :: Bits a => a
allBits = complement zeroBits

invert :: Bits a => a -> a
invert = (allBits `xor`)

infixl 4 \\\
(\\\) :: Bits a => BitSet a -> BitSet a -> BitSet a
BitSet bs1 \\\ BitSet bs2 = BitSet (bs1 .&. invert bs2)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

infixl 1 `isSubsetOf`
isSubsetOf :: Bits a => BitSet a -> BitSet a -> Bool
isSubsetOf = isEmpty .: (\\\)