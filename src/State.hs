module State (
  ST (..),
  startState,
  getPiece,
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
  [ Just (Piece ChessBlack R False), Just (Piece ChessBlack N False), 
    Just (Piece ChessBlack B False), Just (Piece ChessBlack Q False), 
    Just (Piece ChessBlack K False), Just (Piece ChessBlack B False), 
    Just (Piece ChessBlack N False), Just (Piece ChessBlack R False) ], 
  replicate 8 (Just (Piece ChessBlack P False)),
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 (Just (Piece ChessWhite P False)),
  [ Just (Piece ChessWhite R False), Just (Piece ChessWhite N False), 
    Just (Piece ChessWhite B False), Just (Piece ChessWhite Q False), 
    Just (Piece ChessWhite K False), Just (Piece ChessWhite B False), 
    Just (Piece ChessWhite N False), Just (Piece ChessWhite R False) ]]

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
updateState (start, end) state = do 
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
updatePiece (x,_:ys) piece = x ++ setMoved piece : ys
  where
    setMoved :: Maybe Piece -> Maybe Piece
    setMoved Nothing = Nothing
    setMoved (Just p) = Just (p { moved = True })