import Data.Char (intToDigit, digitToInt)
import Data.Maybe (catMaybes)
import qualified Data.Vector as V
import Data.Vector ( (!) ) 
import qualified Data.Set as Set

type Block = V.Vector (Maybe Int)

data Sudoku = Sudoku (V.Vector Block) 
  deriving (Eq)
instance Show Sudoku where 
  show (Sudoku rs) = 
    let bufferLine = "+---+---+---+\n"
        lines      = V.map printLine rs
    in bufferLine ++ (lines ! 0) ++ (lines ! 1) ++ (lines ! 2) ++
       bufferLine ++ (lines ! 3) ++ (lines ! 4) ++ (lines ! 5) ++
       bufferLine ++ (lines ! 6) ++ (lines ! 7) ++ (lines ! 8) ++
       bufferLine 
    where printLine :: Block -> String
          printLine row = 
            let r = V.map cellToChar row 
            in '|' : (r ! 0) : (r ! 1) : (r ! 2) :
               '|' : (r ! 3) : (r ! 4) : (r ! 5) :
               '|' : (r ! 6) : (r ! 7) : (r ! 8) :
               '|' : '\n' : [] 
  
{- -                                                                - -}
{-                       Check Solution                               -}
{- -                                                                - -}
isCellChar :: Char -> Bool
isCellChar c = Prelude.elem  c "123456789."

cellToChar :: Maybe Int -> Char
cellToChar Nothing  = '.'
cellToChar (Just n) = intToDigit n

readCell :: Char -> Maybe Int
readCell '.' = Nothing
readCell  c  = (Just . digitToInt) c

isSudoku :: Sudoku -> Bool 
isSudoku (Sudoku rows) = (V.length rows == 9) && (allOf $ V.map isRow rows) 
  where isRow row = (V.length row == 9) && (allOf $ V.map isCell row) 
        allOf = V.all (== True)
        isCell Nothing  = True
        isCell (Just n) = n >= 1 && n <= 9   

isSolved :: Sudoku -> Bool
isSolved p@(Sudoku s) = 
  (isSudoku p) &&
  (V.all ((== 9) . V.length) (V.map catMaybesV s))
    where catMaybesV = V.fromList . catMaybes . V.toList 
        
validBlock :: Block -> Bool
validBlock = noDuplicates . catMaybes . V.toList
  where noDuplicates l = trackDupes Set.empty l
        trackDupes _  []     = True
        trackDupes ys (x:xs) = if Set.member x ys
                               then False
                               else trackDupes (Set.insert x ys) xs
blocks :: Sudoku -> [Block]
blocks (Sudoku grid) = 
  let rows    = V.toList grid
      cols    = [V.fromList [grid!r!c | r <- [0..8]] | c <- [0..8]]
      squares = [ V.fromList 
                     [grid!(r+r')!(c+c') | r' <- [-1,0,1], 
                                           c' <- [-1,0,1]]
                     | r  <- [1,4,7], c  <- [1,4,7] ]
  in rows ++ cols ++ squares

validState :: Sudoku -> Bool
validState = (all (== True)) . (map validBlock) . blocks

validSudoku :: Sudoku -> Bool
validSudoku s = isSolved s && validState s
{- -                                                                - -}


{- -                                                                - -}
{-                          Backtracking                              -}
{- -                                                                - -}
{-
solveSudoku :: Sudoku -> Maybe Sudoku
solveSudoku s = 
  if validSudoku s
  then s
  else
-}
{- -                                                                - -}


{- -                                                                - -}
{-                             Sudokus                                -}
{- -                                                                - -}
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
                             "4..56..9.",
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
