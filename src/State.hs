module State where

import Board
import Piece

newtype ST = S { apply :: ( Pos, Maybe Piece ) -> Board -> (Maybe Piece, Board) }

getPiece :: Board -> Pos -> Maybe Piece
getPiece board (x, y) =  piece (board !! x !! y )

move :: ST
move = S $ \((x,y), p) board -> (
    getPiece board (x,y),
    updateBoard (x,y) p board
  )

updateBoard :: Pos -> Maybe Piece -> Board -> Board
updateBoard (x,y) p board = [[
  Square (i,j) (if (i,j) == (x,y) then p else getPiece board (j,i)) (if even (i + j) then ChessWhite else ChessBlack) | i <- [0..7]] | j <- [0..7]]