module State where

import Board
import Piece

{--
  State object is only used after the move denoted by (Pos, Pos) has been
  proven to be valid. It returns the updated board state after the move
  and the captured piece if there is one.
--}
newtype ST = S { apply :: (Pos, Pos) -> Board -> (Maybe Piece, Board) }

-- Return piece on the square denoted by the Pos arg
getPiece :: Board -> Pos -> Maybe Piece
getPiece board (x, y) =  piece (board !! x !! y )

-- Perform valid move, returning the updated board state and either
-- a captured piece or nothing
move :: ST
move = S $ \((a,b), (c,d)) board -> ( 
    case ((a,b), (c,d)) of
      ((i,j), (h,k)) | (i,j) == (h,k) -> (Nothing, board) 
      ((i,j), (h,k)) |
        (i >= 0 && i <= 7) && (j >= 0 && j <= 7) &&
        (h >= 0 && h <= 7) && (k >= 0 && k <= 7) -> 
          (
            getPiece board (c,d),
            updateBoard ((a,b), (c,d)) board
          )
      ((_, _), (_, _)) -> (Nothing, board)
  )

-- Update the board state by instantiating a duplicate of every square
-- except the squares the player is moving from and to.
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
