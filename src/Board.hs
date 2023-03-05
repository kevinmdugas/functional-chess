module Board where

import Piece

data Square = Square { piece :: Maybe Piece, tile :: ChessColor } deriving (Show, Eq)
type Board = [[Square]]

emptyBoard :: Board
emptyBoard = [[Square Nothing (if (i + j) `mod` 2 == 0 then ChessWhite else ChessBlack) | i <- [0..7]] | j <- [0..7]]

updateBoard :: Board -> [[Maybe Piece]] -> Board
updateBoard board boardState = zipWith (zipWith updateSquare) board boardState

updateSquare :: Square -> Maybe Piece -> Square
updateSquare square maybePiece = square { piece = maybePiece }

startState :: [[Maybe Piece]]
startState = [
  [ Just (Piece ChessBlack R), Just (Piece ChessBlack N), 
    Just (Piece ChessBlack B), Just (Piece ChessBlack Q), 
    Just (Piece ChessBlack K), Just (Piece ChessBlack B), 
    Just (Piece ChessBlack N), Just (Piece ChessBlack R) ], 
    replicate 8 (Just (Piece ChessBlack P)),
    replicate 8 (Nothing),
    replicate 8 (Nothing),
    replicate 8 (Nothing),
    replicate 8 (Nothing),
    replicate 8 (Just (Piece ChessWhite P)),
    [ Just (Piece ChessWhite R), Just (Piece ChessWhite N), 
      Just (Piece ChessWhite B), Just (Piece ChessWhite Q), 
      Just (Piece ChessWhite K), Just (Piece ChessWhite B), 
      Just (Piece ChessWhite N), Just (Piece ChessWhite R) ] ]