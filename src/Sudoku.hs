module Sudoku where

import Boards

-- If cell's possible is length 1, fill in guess
evalCell :: Cell -> Cell
evalCell (Cell _ [x]) = Cell (Just x) []
evalCell (Cell i p) = Cell i p

-- Fill in possibles for a board (column + row only)
fillInPossibleBoard :: [[Cell]] -> [[Cell]]
fillInPossibleBoard board =
  let rowPossibles = fmap fillInPossible board
      columnPossibles = fmap (getColumnPossibles board) [0 .. 8]
   in zipColumnPossibles rowPossibles columnPossibles

-- Given a board and a 2D array of column possibles, return a board with possibles filled out
zipColumnPossibles :: [[Cell]] -> [[Int]] -> [[Cell]]
zipColumnPossibles = zipWith f
  where
    f row possible = fmap (integratePossibilities possible) row

-- Given a board and a column number, get the possibles
getColumnPossibles :: [[Cell]] -> Int -> [Int]
getColumnPossibles board c =
  getPossible $ do
    row <- board
    let cell = row !! c
    getFilledIn (return cell)

-- Fill in all possible guesses for a row (row only)
fillInPossible :: [Cell] -> [Cell]
fillInPossible row =
  let filledIn = getPossible $ getFilledIn row
   in fmap (replacePoss filledIn) row

-- Given a list of possible and a cell, replace the cell's possible with the list
replacePoss :: [Int] -> Cell -> Cell
replacePoss possible (Cell g p) = Cell g possible

-- Given a list of possible and a cell, set the cell's possible to the union of the two
integratePossibilities :: [Int] -> Cell -> Cell
integratePossibilities possible (Cell g p) = Cell g (possible `union` p)

-- Given a row, get all the already guessed
getFilledIn :: [Cell] -> [Int]
getFilledIn = foldr f []
  where
    f (Cell (Just x) _) list = x : list
    f _ list = list

-- Given two lists, get elemens in both
union :: [Int] -> [Int] -> [Int]
union xs ys =
  foldr
    (\a b ->
       if a `elem` ys
         then a : b
         else b)
    []
    xs

-- Given already filled in, get remaining of the digits
getPossible :: [Int] -> [Int]
getPossible xs = [x | x <- [1 .. 9], not $ elem x xs]

-- Given guesses, fill them in
evalBoard :: [[Cell]] -> [[Cell]]
evalBoard = fmap . fmap $ evalCell
