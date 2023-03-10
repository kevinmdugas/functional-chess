module State (
  ST (..),
  startState,
  move,
) where

import Board

{--
  State object is only used after the move denoted by (Pos, Pos) has been
  proven to be valid. It returns the updated board state after the move
  and the captured piece if there is one.
--}
newtype ST = S { apply :: (Pos, Pos) -> GameState -> (Maybe Piece, GameState) }

startState :: GameState
startState = [
  [ Just (Piece ChessBlack R), Just (Piece ChessBlack N), 
    Just (Piece ChessBlack B), Just (Piece ChessBlack Q), 
    Just (Piece ChessBlack K), Just (Piece ChessBlack B), 
    Just (Piece ChessBlack N), Just (Piece ChessBlack R) ], 
  replicate 8 (Just (Piece ChessBlack P)),
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 (Just (Piece ChessWhite P)),
  [ Just (Piece ChessWhite R), Just (Piece ChessWhite N), 
    Just (Piece ChessWhite B), Just (Piece ChessWhite Q), 
    Just (Piece ChessWhite K), Just (Piece ChessWhite B), 
    Just (Piece ChessWhite N), Just (Piece ChessWhite R) ]]

getPiece :: GameState -> Pos -> Maybe Piece
getPiece state (x, y) =  state !! x !! y

getRow :: GameState -> Int -> [Maybe Piece]
getRow state i = state !! i

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
  x | x == a && x == c -> do
    let row' = updatePiece (splitAt b (getRow state a)) Nothing
    updatePiece (splitAt d row') (getPiece state (a,b))
  x | x == a -> updatePiece (splitAt b (getRow state a)) Nothing
  x | x == c -> updatePiece (splitAt d (getRow state c)) (getPiece state (a,b))
  _          -> getRow state i

updatePiece :: ([Maybe Piece], [Maybe Piece]) -> Maybe Piece -> [Maybe Piece]
updatePiece (x,_:ys) piece = x ++ piece : ys