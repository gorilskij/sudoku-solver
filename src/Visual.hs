module Visual (showSudoku, showSudoku') where

import LibBase
import BitSet

import Data.List (intercalate)
import Data.List.Split (chunksOf)

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