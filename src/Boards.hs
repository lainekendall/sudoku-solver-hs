module Boards where

import Text.Printf

--               guess       possible
data Cell = Cell (Maybe Int) [Int] deriving (Eq, Show)

testBoard = 
  [
    [
      Cell (Just 2) [], Cell Nothing [], Cell Nothing [], Cell Nothing [],
      Cell Nothing [], Cell (Just 9) [], Cell Nothing [], Cell Nothing [],
      Cell Nothing []
    ],
    [
      Cell Nothing [], Cell (Just 4) [], Cell Nothing [], Cell Nothing [],
      Cell Nothing [], Cell Nothing [], Cell (Just 1) [], Cell Nothing [],
      Cell Nothing []
    ],
    [
      Cell Nothing [], Cell Nothing [], Cell Nothing [], Cell Nothing [],
      Cell Nothing [], Cell (Just 3) [], Cell Nothing [], Cell Nothing [],
      Cell Nothing []
    ],
    [
      Cell Nothing [], Cell Nothing [], Cell Nothing [], Cell Nothing [],
      Cell Nothing [], Cell Nothing [], Cell Nothing [], Cell Nothing [],
      Cell Nothing []
    ],
    [
      Cell Nothing [], Cell Nothing [], Cell Nothing [], Cell Nothing [],
      Cell Nothing [], Cell Nothing [], Cell Nothing [], Cell Nothing [],
      Cell Nothing []
    ],
    [
      Cell Nothing [], Cell Nothing [], Cell (Just 7) [], Cell Nothing [],
      Cell Nothing [], Cell Nothing [], Cell Nothing [], Cell Nothing [],
      Cell (Just 2) []
    ],
    [
      Cell Nothing [], Cell Nothing [], Cell Nothing [], Cell Nothing [],
      Cell (Just 5) [], Cell Nothing [], Cell Nothing [], Cell Nothing [],
      Cell Nothing []
    ],
    [
      Cell (Just 7) [], Cell Nothing [], Cell Nothing [], Cell (Just 5) [],
      Cell Nothing [], Cell (Just 8) [], Cell Nothing [], Cell Nothing [],
      Cell Nothing []
    ],
    [
      Cell Nothing [], Cell Nothing [], Cell (Just 3) [], Cell (Just 6) [],
      Cell Nothing [], Cell Nothing [], Cell Nothing [], Cell (Just 9) [],
      Cell Nothing []
    ]
  ]

prettyPrint :: [[Cell]] -> String
prettyPrint board = concat [(printRow x) ++ "\n" | x <- board]

prettyPrintGuesses board = concat [(printGuesses x) ++ "\n" | x <- board]

printRow :: [Cell] -> String
printRow row = concat ["[ " ++ unwrap x ++ " ]" | (Cell x _) <- row]
printGuesses row = concat ["[ " ++ unwrap x ++ show p ++ " ]" | (Cell x p) <- row]

unwrap Nothing = " "
unwrap (Just x) = show x
pPrint b = printf $ prettyPrint b
