module State where

import Board
import Piece

{--
  State object is only used after the move denoted by (Pos, Pos) has been
  proven to be valid. It returns the updated board state after the move
  and the captured piece if there is one.
--}
newtype ST = S { apply :: (Pos, Pos) -> Board -> (Maybe Piece, Board) }

-- Perform valid move, returning the updated board state and either
-- a captured piece or nothing
move :: ST
move = S $ \(start, end) board -> (
  case (start, end) of
    (start, end) | start == end -> (Nothing, board) 

    ((i,j), (h,k)) | inRange i && inRange j && inRange h && inRange k ->
      (getPiece board end, updateBoard (start, end) board)

    (_, _) -> (Nothing, board)
  ) where
    inRange :: Int -> Bool
    inRange x = x >= 0 && x <= 7

-- Update the board state by instantiating a duplicate of every square
-- except the squares the player is moving from and to.
updateBoard :: (Pos, Pos) -> Board -> Board
updateBoard (cur, dest) board = 
  [[ Square
    (
      if (i,j) == cur then Nothing
      else if (i,j) == dest then getPiece board cur
      else getPiece board (i,j)
    )
    (if even (i + j) then ChessWhite else ChessBlack) 
    | i <- [0..length (head board) - 1]]
    | j <- [0..length board - 1]]
