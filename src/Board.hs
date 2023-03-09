module Board where

import Piece

type Pos = (Int, Int)

data Square = Square { 
  piece :: Maybe Piece,
  tile  :: ChessColor
} deriving (Show, Eq)

type Board = [[Square]]
type GameState = [[Maybe Piece]]

emptyBoard :: Board
emptyBoard = [[Square Nothing 
  (if even (i + j) then ChessWhite else ChessBlack) | i <- [0..7]] | j <- [0..7]]

startState :: [[Maybe Piece]]
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

updateBoard :: Board -> GameState -> Board
updateBoard = zipWith (zipWith updateSquare)

updateSquare :: Square -> Maybe Piece -> Square
updateSquare square maybePiece = square { piece = maybePiece }
