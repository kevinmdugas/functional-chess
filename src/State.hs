module State where

import Board
import Piece

{--
  State object is only used after the move denoted by (Pos, Pos) has been
  proven to be valid. It returns the updated board state after the move
  and the captured piece if there is one.
--}
newtype ST = S { apply :: (Pos, Pos) -> GameState -> (Maybe Piece, GameState) }

move :: ST
move = S $ \(start, end) state -> (
  case (start, end) of
    (start, end) | start == end -> (Nothing, state) 

    ((i,j), (h,k)) | inRange i && inRange j && inRange h && inRange k ->
      (getPiece state end, updateState (start, end) state)

    (_, _) -> (Nothing, state)
  ) where
    inRange :: Int -> Bool
    inRange x = x >= 0 && x <= 7

updateState :: (Pos, Pos) -> GameState -> GameState
updateState (start, end) state = 
  [ updateRow (start, end) state row | row <- [0..7] ] 

updateRow :: (Pos, Pos) -> GameState -> Int -> [Maybe Piece]
updateRow ((a,b), (c,d)) state i =  case i of
  x | x == a -> updatePiece (splitAt b (getRow state a)) Nothing
  x | x == c -> updatePiece (splitAt d (getRow state c)) (getPiece state (a,b))
  _          -> getRow state i

updatePiece :: ([Maybe Piece], [Maybe Piece]) -> Maybe Piece -> [Maybe Piece]
updatePiece (x,_:ys) piece = x ++ piece : ys