module Board where

import Piece

type Pos = (Int, Int)

data Square = Square { 
  piece :: Maybe Piece,
  tile  :: ChessColor
} deriving (Show, Eq)

type Board = [[Square]]

getPiece :: Board -> Pos -> Maybe Piece
getPiece board (x, y) =  piece (board !! y !! x )

getTile :: Board -> Pos -> ChessColor
getTile board (x, y) = tile (board !! y !! x)

startState :: Board
startState = [[
  Square (determinePiece i j)
  (if even (i + j) then ChessWhite else ChessBlack) | i <- [0..7]] | j <- [0..7]]

determinePiece :: Int -> Int -> Maybe Piece
determinePiece i j = case (i, j) of
  (0, c) | c == 0 || c == 7 -> Just (Piece ChessBlack R)
  (0, c) | c == 1 || c == 6 -> Just (Piece ChessBlack N)
  (0, c) | c == 2 || c == 5 -> Just (Piece ChessBlack B)
  (0, 3)                    -> Just (Piece ChessBlack Q)
  (0, 4)                    -> Just (Piece ChessBlack K)
  (1, _)                    -> Just (Piece ChessBlack P)
  (6, _)                    -> Just (Piece ChessWhite P)
  (7, c) | c == 0 || c == 7 -> Just (Piece ChessWhite R)
  (7, c) | c == 1 || c == 6 -> Just (Piece ChessWhite N)
  (7, c) | c == 2 || c == 5 -> Just (Piece ChessWhite B)
  (7, 3)                    -> Just (Piece ChessWhite Q)
  (7, 4)                    -> Just (Piece ChessWhite K)
  (_, _)                    -> Nothing
