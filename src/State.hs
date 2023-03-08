module State where

import Board
import Piece

newtype ST = S { apply :: (Pos, Pos) -> Board -> (Maybe Piece, Board) }

getPiece :: Board -> Pos -> Maybe Piece
getPiece board (x, y) =  piece (board !! x !! y )

move :: ST
move = S $ \(cur, dest) board -> (
    getPiece board dest,
    updateBoard (cur, dest) board
  )

updateBoard :: (Pos, Pos) -> Board -> Board
updateBoard (cur, dest) board = [[
  Square
    (j,i) 
    (
      if (j,i) == cur then Nothing
      else if (j,i) == dest then getPiece board cur
      else getPiece board (j,i)
    )
    (if even (j + i) then ChessWhite else ChessBlack) | i <- [0..7]] | j <- [0..7]]
