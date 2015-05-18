import Sudoku
import qualified Data.Vector as V

allBlankSudoku, easySudoku, midSudoku, hardSudoku, evilSudoku :: Sudoku
 
allBlankSudoku = Sudoku $ V.fromList $ replicate 9 $ V.fromList $ replicate 9 Nothing

strList2Sudoku :: [String] -> Sudoku
strList2Sudoku l = Sudoku $ V.fromList $
                            map (\row -> V.fromList $ map readCell row) l

easySudoku = strList2Sudoku ["3.9....42",
                             ".189436..",
                             "......89.",
                             "..3.9..6.",
                             "427...589",
                             ".6..8.2..",
                             ".72......",
                             "..457632.",
                             "63....7.4"]
 
midSudoku  = strList2Sudoku ["...12..3.",
                             "..3.8..16",
                             "4..53..9.",
                             ".1.8..52.",
                             ".4.....6.",
                             ".68..2.7.",
                             ".8..93..2",
                             "69..5.3..",
                             ".3..48..."]

hardSudoku = strList2Sudoku ["6..8.9...",
                             "..5..7.86",
                             ".7.......",
                             "...4.13.7",
                             "8.1...5.4",
                             "7.92.5...",
                             ".......4.",
                             "18.5..6..",
                             "...3.4..5"]

evilSudoku = strList2Sudoku ["1........",
                             "7....81.2",
                             ".63.5....", 
                             ".7.39....",
                             "..58.46..",
                             "....25.4.",
                             "....1.87.",
                             "28.9....3",
                             "........6"]

{- -                                                                - -}
 
main :: IO ()
main = putStrLn $ "Easy:\n"   ++ solveSudoku easySudoku ++
                  "Medium:\n" ++ solveSudoku midSudoku  ++
                  "Hard:\n"   ++ solveSudoku hardSudoku ++
                  "Evil:\n"   ++ solveSudoku evilSudoku

