module TestData where

import Board
import Piece

initTestBoard :: Board
initTestBoard = [
    [ 
      Square (Just (Piece ChessBlack R)) ChessBlack,
      Square (Just (Piece ChessBlack N)) ChessWhite
    ],
    [
      Square (Just (Piece ChessBlack P)) ChessWhite,
      Square (Just (Piece ChessBlack P)) ChessBlack
    ],
    replicate 2 (Square Nothing ChessWhite),
    [
      Square (Just (Piece ChessWhite P)) ChessWhite,
      Square (Just (Piece ChessWhite P)) ChessBlack
    ],
    [ 
      Square (Just (Piece ChessWhite Q)) ChessWhite,
      Square (Just (Piece ChessWhite K)) ChessBlack
    ]
  ]