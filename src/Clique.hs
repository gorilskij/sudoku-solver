module Clique (cliques, disallowCliques) where

import LibBase
import BitSet

import Data.Maybe (mapMaybe, catMaybes)
import Data.List (foldl')
import Control.Applicative (liftA2)
import Combinatorics (partitions)

data CliqueType = Type1 | Type2 | Type3 deriving (Show, Eq)
type CliqueData = (Indices, Allowed) -- (ids, values)
data Clique = Clique { cliqueType :: CliqueType
                     , creationGroup :: GroupId
                     , containingGroups :: [GroupId]
                     , cliqueData :: CliqueData
                     } deriving Show

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

-- type (rows, cols, squares) to ignore -> set of indices -> ...
{-# INLINE containingGroupsExcept #-}
containingGroupsExcept :: GroupId -> Indices -> [GroupId]
containingGroupsExcept g is = find $ setsExcept g
    where find = map fst . take 1 . filter ((is `isSubsetOf`) . snd)
          setsExcept :: GroupId -> [(GroupId, Indices)]
          -- no size-1 cliques, remember, rows and cols don't mix
          setsExcept (Row _) = squareIndices
          setsExcept (Col _) = squareIndices
          setsExcept (Square _) = rowIndices ++ colIndices

type BareEmptyICell = (Index, Allowed)

-- _ -> (other group members, potential clique) -> ...
{-# INLINE tryIntoClique #-}
tryIntoClique :: GroupId -> ([BareEmptyICell], [BareEmptyICell]) -> MaybeFeasible (Maybe Clique)
tryIntoClique gId (otherIAs, ias) = let (is, as) = unzip ias
                                        is' = fromList is
                                        cGs = containingGroupsExcept gId is'

                                        (a':as') = as
                                        isClique1 = length is == len a' && all (a' ==) as'
     
                                        sharedAs = intersections as
                                        otherAs = unions $ map snd otherIAs
                                        uniqueAs = sharedAs \\\ otherAs
                                    in if isClique1
                                           then Feasible $ Just $ Clique Type1 gId cGs (is', a')
                                           else case len uniqueAs `compare` length is of
                                                    LT -> if len uniqueAs == 0
                                                              then Feasible $ Nothing -- a lot of these
                                                              else Feasible $ Just $ Clique Type3 gId cGs (is', uniqueAs)
                                                    EQ -> Feasible $ Just $ Clique Type2 gId cGs (is', uniqueAs)
                                                    GT -> Infeasible -- means the sudoku is unsolvable in this state

safeInit [] = []
safeInit l  = init l

{-# INLINE cliques' #-}
cliques' :: Group -> MaybeFeasible [Clique]
cliques' (gId, g) = fmap catMaybes
                  $ sequence -- for feasibility
                  $ map (tryIntoClique gId)
                  $ safeInit -- ignore full group clique
                  -- could be useful to ignore "number of free cells in the group"-sized cliques
                  -- consider also removing cliques where the size == the number of empty cells in the group
                  $ dropWhile ((< 2) . length . snd) -- without single element cliques
                  -- not that [(big->small, small->big)]
                  $ partitions -- [(others, subsequence)]
                  $ mapMaybe unwrapEmpty
                  $ g

cliques :: Sudoku -> MaybeFeasible [Clique]
cliques s -- maybe nub here somehow
        = rowCs +++ colCs +++ squareCs
    where sCs = (fmap concat) . sequence . map cliques'
          rowCs = sCs s
          colCs = sCs $ rows2cols s
          squareCs = sCs $ rows2squares s
          (+++) = liftA2 (++)

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
{-# INLINE disallowCliqueA #-}
disallowCliqueA :: CliqueData -> [ICell] -> [ICell]
disallowCliqueA (is, vs) g = map removeVsIfOther g
    where removeVsIfOther (i, Empty as)
              | not $ i `member` is = (i, Empty (as \\\ vs))
          removeVsIfOther c = c

-- disallow all non-clique values from clique members
-- assumes (g `contains` c2)
{-# INLINE disallowCliqueB #-}
disallowCliqueB :: CliqueData -> [ICell] -> [ICell]
disallowCliqueB (is, vs) g = map removeAsIfMember g
    where removeAsIfMember (i, Empty as)
              | i `member` is = (i, Empty vs)
          removeAsIfMember c = c

-- do we need disallow at all?
{-# INLINE disallowClique' #-}
disallowClique' :: Clique -> Group -> Group
disallowClique' Clique{cliqueType=Type1, cliqueData=cd} (gId, g) = (gId, disallowCliqueA cd g)

disallowClique' Clique{cliqueType=Type2, creationGroup=cId, cliqueData=cd} gr@(gId, g)
    | cId == gId = (gId, disallowCliqueB cd g)
    | otherwise  = (gId, disallowCliqueA cd $ disallowCliqueB cd g)

disallowClique' Clique{cliqueType=Type3, creationGroup=cId, cliqueData=cd} gr@(gId, g)
    | cId == gId = gr
    | otherwise  = (gId, disallowCliqueA cd g)

{-# INLINE disallowCliques' #-}
disallowCliques' :: [Clique] -> Group -> Group
disallowCliques' cs g = applyAll (map disallowClique' myCs) g
    where myCs = filter (g `contains`) cs

disallowCliques :: Sudoku -> MaybeFeasible Sudoku
disallowCliques s = case cliques s of
                        Infeasible -> Infeasible
                        Feasible cs
                            | length cs == 0 -> Feasible s
                            | otherwise      -> pure
                                              $ map (disallowCliques' cs) -- rows
                                              $ cols2rows
                                              $ map (disallowCliques' cs) -- cols
                                              $ rows2cols
                                              $ squares2rows
                                              $ map (disallowCliques' cs) -- squares
                                              $ rows2squares
                                              $ s