module Sudoku ( Sudoku (..), solveSudoku, readCell ) where

import Data.Char (intToDigit, digitToInt)
import Data.Maybe (catMaybes, isNothing)
import qualified Data.Vector as V
import qualified Data.Set as Set

(!) :: V.Vector a -> Int -> a
(!) = V.unsafeIndex

type Block = V.Vector (Maybe Int)

data Sudoku = Sudoku (V.Vector Block) 
  deriving (Eq)
instance Show Sudoku where 
  show (Sudoku rs) = 
    let bufferLine = "+---+---+---+\n"
        lines      = V.map printLine rs
    in bufferLine ++ (lines!0) ++ (lines!1) ++ (lines!2) ++
       bufferLine ++ (lines!3) ++ (lines!4) ++ (lines!5) ++
       bufferLine ++ (lines!6) ++ (lines!7) ++ (lines!8) ++
       bufferLine 
    where printLine :: Block -> String
          printLine row = 
            let r = V.map cellToChar row 
            in '|' : (r!0) : (r!1) : (r!2) :
               '|' : (r!3) : (r!4) : (r!5) :
               '|' : (r!6) : (r!7) : (r!8) :
               '|' : '\n' : [] 
  
{- -                                                                - -}
{-                       Check Solution                               -}
{- -                                                                - -}
isCellChar :: Char -> Bool
isCellChar c = elem  c "123456789."

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
solveSudoku :: Sudoku -> String
solveSudoku s = case recurseSolve s of
                (Just s) -> show s
                Nothing  -> "No solution possible :("

recurseSolve :: Sudoku -> Maybe Sudoku
recurseSolve s = 
  if   validSudoku s
  then Just s
  else 
    let cell    = getCell s 
        options = getCellOptions s cell
    in assign s cell options          

assign :: Sudoku -> (Int, Int) -> [Maybe Int] -> Maybe Sudoku
assign _ _    []     = Nothing
assign s cell (n:ns) = 
  let nextState = cellUpdate s cell n
  in  if   validState nextState
      then case recurseSolve nextState of
           Nothing    -> assign s cell ns
           s@(Just _) -> s
      else assign s cell ns 
       
getCell :: Sudoku -> (Int, Int)
getCell (Sudoku rows) = 
  head [(r,c) | r <- [0..8], c <- [0..8], isNothing (rows!r!c)]

getCellOptions :: Sudoku -> (Int, Int) -> [Maybe Int]
getCellOptions _ _ = map Just [1..9]

cellUpdate :: Sudoku -> (Int, Int) -> Maybe Int -> Sudoku
cellUpdate (Sudoku rows) (r,c) n = 
  let enum    = V.zip (V.fromList [0..9]) 
      newRow  = V.map (\(i, v) -> if i == c then n      else v)  
                      $ enum (rows!r)   
      newRows = V.map (\(i, v) -> if i == r then newRow else v)  
                      $ enum rows
  in  Sudoku newRows
