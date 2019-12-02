module Spec where

import Boards
import Sudoku
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "getPossible" $ do
    it "Empty" $ getPossible [] == [1 .. 9]
    it "one" $ getPossible [1] == [2 .. 9]
    it "two" $ getPossible [1, 5] == [2 .. 4] ++ [6 .. 9]
  describe "union" $ do
    it "Empty" $ null $ union [] []
    it "more" $ union [1, 6, 9] [1 .. 9] == [1, 6, 9]
    it "more" $ union [1 .. 9] [2, 4, 8] == [2, 4, 8]
  describe "getFilledIn" $ do
    it "Empty" $ null $ getFilledIn []
    it "first row" $ getFilledIn (head testBoard) == [2, 9]
  describe "integratePossiblities" $ do
    it "Empty" $ integratePossibilities [] (Cell Nothing []) == Cell Nothing []
    it "some" $
      integratePossibilities [1 .. 7] (Cell Nothing [5 .. 9]) ==
      Cell Nothing [5 .. 7]
    it "none" $
      integratePossibilities [] (Cell Nothing [1 .. 9]) == Cell Nothing []
  describe "replacePoss" $ do
    it "Empty" $ replacePoss [] (Cell Nothing [1 .. 9]) == Cell Nothing []
    it "some" $
      replacePoss [2 .. 7] (Cell Nothing [5, 6]) == Cell Nothing [2 .. 7]
  describe "fillInPossible" $ do
    it "Empty" $ null $ fillInPossible []
    it "some" $
      head (fillInPossible (head testBoard)) == Cell (Just 2) (1 : [3 .. 8])
  describe "getColumnPossibles" $ do
    it "Empty" $ getColumnPossibles [] 0 == [1 .. 9]
    it "some" $ getColumnPossibles testBoard 0 == [1, 3, 4, 5, 6, 8, 9]
