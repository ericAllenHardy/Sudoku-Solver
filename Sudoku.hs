module Sudoku ( Sudoku (..), solveSudoku, readCell ) where

import Data.Char (intToDigit, digitToInt)
import Data.Maybe (catMaybes, isNothing, isJust, fromJust)
import qualified Data.Vector.Persistent as V
--import qualified Data.Set as Set
import Data.List (nub, minimumBy, transpose)
import Data.Foldable (toList)
import Control.Monad (guard)
import Data.Ord (compare)


(!) :: V.Vector a -> Int -> a
(!) = V.unsafeIndex
allV :: (a -> Bool) -> V.Vector a -> Bool
allV p = V.foldl' (\x y -> x && p y) True

anyV :: (a -> Bool) -> V.Vector a -> Bool
anyV p = V.foldl' (\x y -> x || p y) False

chunksOf :: Int -> [a] -> [[a]]
chucksOf _ [] = []
chunksOf n l  = (take n l) : (chunksOf n $ drop n l)

type Block = [(Maybe Int)]

data Sudoku = Sudoku (V.Vector (V.Vector (Maybe Int))) 
  deriving (Eq)
instance Show Sudoku where 
  show (Sudoku rs) = 
    let bufferLine = "+---+---+---+\n"
        lines      = V.map printLine rs
    in bufferLine ++ (lines!0) ++ (lines!1) ++ (lines!2) ++
       bufferLine ++ (lines!3) ++ (lines!4) ++ (lines!5) ++
       bufferLine ++ (lines!6) ++ (lines!7) ++ (lines!8) ++
       bufferLine 
    where printLine :: V.Vector (Maybe Int) -> String
          printLine row = 
            let r = V.map cellToChar row 
            in '|' : (r!0) : (r!1) : (r!2) :
               '|' : (r!3) : (r!4) : (r!5) :
               '|' : (r!6) : (r!7) : (r!8) :
               '|' : '\n' : [] 


type CellOptions = [Int]
isSet, isInvalid :: CellOptions -> Bool
isSet = (== 1) . length
isInvalid = (== [])
dropOption :: Int -> CellOptions -> CellOptions
dropOption n = filter (/= n)

data OptionGrid = Grid (V.Vector (V.Vector CellOptions))
instance Show OptionGrid where
  show (Grid rows) = 
    let gridLine = "+-----------+-----------+-----------+\n"
        cellLine = "|   *   *   |   *   *   |   *   *   |\n"
        rowsString = V.map showRow rows 
    in gridLine ++ (rowsString!0!0) ++ (rowsString!0!1) ++ (rowsString!0!2) ++ 
       cellLine ++ (rowsString!1!0) ++ (rowsString!1!1) ++ (rowsString!1!2) ++
       cellLine ++ (rowsString!2!0) ++ (rowsString!2!1) ++ (rowsString!2!2) ++
       gridLine ++ (rowsString!3!0) ++ (rowsString!3!1) ++ (rowsString!3!2) ++
       cellLine ++ (rowsString!4!0) ++ (rowsString!4!1) ++ (rowsString!4!2) ++
       cellLine ++ (rowsString!5!0) ++ (rowsString!5!1) ++ (rowsString!5!2) ++
       gridLine ++ (rowsString!6!0) ++ (rowsString!6!1) ++ (rowsString!6!2) ++
       cellLine ++ (rowsString!7!0) ++ (rowsString!7!1) ++ (rowsString!7!2) ++
       cellLine ++ (rowsString!8!0) ++ (rowsString!8!1) ++ (rowsString!8!2) ++
       gridLine
    where showCell :: [Int] -> [String]
          showCell cell = chunksOf 3 $ map (showOpt cell) [1..9]
          showOpt :: [Int] -> Int -> Char 
          showOpt cell n = if n `elem` cell then intToDigit n else '.'
          showRow :: V.Vector [Int] -> V.Vector String
          showRow row =  
            let cellBlocks = V.map showCell row
            in V.fromList $
               ["|" ++ (cellBlocks!0!!x) ++ " " ++ (cellBlocks!1!!x) ++
                " " ++ (cellBlocks!2!!x) ++ "|" ++ (cellBlocks!3!!x) ++
                " " ++ (cellBlocks!4!!x) ++ " " ++ (cellBlocks!5!!x) ++
                "|" ++ (cellBlocks!6!!x) ++ " " ++ (cellBlocks!7!!x) ++
                " " ++ (cellBlocks!8!!x) ++ "|\n"
                | x <- [0..2]]

createOptions :: Sudoku -> OptionGrid
createOptions (Sudoku rows) = 
  let fullGrid :: OptionGrid
      fullGrid = Grid $ V.fromList $ replicate 9 
                           $ V.fromList $ replicate 9 [1..9]
      givens = gridToCoords rows  
  in  foldl (\x (r,c,n) -> optUpdate x (r,c) n) fullGrid givens

gridToCoords :: V.Vector (V.Vector (Maybe Int)) -> [(Int, Int, Int)]
gridToCoords rows = do r <- [0..8]
                       c <- [0..8]
                       let n = rows!r!c
                       guard $ isJust n 
                       return (r,c, fromJust n) 

deadEnd :: OptionGrid -> Bool
deadEnd (Grid rows) = anyV (== True) $ V.map (anyV (== [])) rows 

optUpdate :: OptionGrid -> (Int, Int) -> Int -> OptionGrid
optUpdate (Grid rows) (r, c) n =
  let newRow   = let row = rows!r
                 in (r, V.update c [n] $ V.map dropN row)
      newCol   = do r' <- [0..(r-2)]++[(r+2)..8]
                    let row = rows!r'
                    return (r', V.update c (dropN $ row!c) row)
      newBlock = do r' <- map (+ 3*(r `div` 3)) [0,2]
                    let row = rows!r'
                        updates = [(c', dropN $ row!c') 
                                   |c' <- map (+ 3*(c `div` 3)) [0..2]]
                    return (r', row V.// updates)
      newOpts = rows V.// (newRow : newCol ++ newBlock)
  in  Grid $ newOpts
  where dropN = dropOption n

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
        allOf = allV (== True)
        isCell Nothing  = True
        isCell (Just n) = n >= 1 && n <= 9   

isSolved :: Sudoku -> Bool
isSolved p@(Sudoku s) = 
  (isSudoku p) &&
  (allV ((== 9) . V.length) (V.map catMaybesV s))
    where catMaybesV = V.fromList . catMaybes . toList 
        
validBlock :: Block -> Bool
validBlock = noDuplicates . catMaybes 
  where noDuplicates l = l == nub l 

blocks :: Sudoku -> [Block]
blocks (Sudoku grid) = 
  let rows    = toList $ V.map toList grid
      cols    = transpose rows
      squares = let f [] = []
                    f ((as):(bs):(cs):xs) = (g as bs cs) ++ (f xs) 
                    g [] [] [] = [] 
                    g (a1:a2:a3:as) (b1:b2:b3:bs) (c1:c2:c3:cs) =
                      [a1,a2,a3,b1,b2,b3,c1,c2,c3] : (g as bs cs)
                in f rows 
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
solveSudoku s = case recurseSolve s (createOptions s) of
                (Just s) -> show s
                Nothing  -> "No solution possible :(\n"

recurseSolve :: Sudoku -> OptionGrid -> Maybe Sudoku
recurseSolve s optGrid = 
  if   validSudoku s
  then Just s
  else if deadEnd optGrid
  then Nothing
  else 
    let cell        = getCell s optGrid 
        cellOptions = getCellOptions optGrid cell
    in assign s optGrid cell cellOptions         

assign :: Sudoku -> OptionGrid -> (Int, Int) -> [Int] -> Maybe Sudoku
assign _ _       _    []     = Nothing
assign s optGrid cell (n:ns) = 
  let nextState  = cellUpdate s cell (Just n)
      newOptGrid = optUpdate optGrid cell n
      nextAssignment = assign s optGrid cell ns
  in  if   validState nextState
      then case recurseSolve nextState newOptGrid of
           Nothing    -> nextAssignment 
           s@(Just _) -> s
      else nextAssignment
       
getCell :: Sudoku -> OptionGrid -> (Int, Int)
getCell (Sudoku grid) (Grid rows) = 
  let cells   = do r <- [0..8]
                   c <- [0..8]
                   guard $ isNothing $ grid!r!c
                   return (r, c, length $ rows!r!c)
      (r,c,_) = minimumBy leastOptions cells 
  in (r,c)
  where 
     leastOptions (_,_,o) (_,_,o') = compare o o'
      
getCellOptions :: OptionGrid -> (Int, Int) -> [Int]
getCellOptions (Grid rows) (r,c) = rows!r!c

cellUpdate :: Sudoku -> (Int, Int) -> Maybe Int -> Sudoku
cellUpdate (Sudoku rows) (r,c) n = 
  let newRow  = V.update c n (rows!r)   
      newRows = V.update r newRow rows
  in  Sudoku newRows
