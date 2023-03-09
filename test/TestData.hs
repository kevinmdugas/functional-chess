module TestData where

import Board
import Piece

testBoard :: Board
testBoard = [
    [ 
      Square (Just (Piece ChessBlack R)) ChessBlack,
      Square (Just (Piece ChessBlack N)) ChessWhite,
      Square (Just (Piece ChessBlack B)) ChessBlack,
      Square (Just (Piece ChessBlack Q)) ChessWhite,
      Square (Just (Piece ChessBlack K)) ChessBlack,
      Square (Just (Piece ChessBlack B)) ChessBlack,
      Square (Just (Piece ChessBlack N)) ChessWhite,
      Square (Just (Piece ChessBlack R)) ChessBlack
    ],
    replicate 8 $ Square (Just (Piece ChessBlack P)) ChessWhite,
    replicate 8 $ Square Nothing ChessWhite,
    replicate 8 $ Square Nothing ChessWhite,
    replicate 8 $ Square Nothing ChessWhite,
    replicate 8 $ Square Nothing ChessWhite,
    replicate 8 $ Square (Just (Piece ChessWhite P)) ChessWhite,
    [ 
      Square (Just (Piece ChessWhite R)) ChessBlack,
      Square (Just (Piece ChessWhite N)) ChessWhite,
      Square (Just (Piece ChessWhite B)) ChessBlack,
      Square (Just (Piece ChessWhite Q)) ChessWhite,
      Square (Just (Piece ChessWhite K)) ChessBlack,
      Square (Just (Piece ChessWhite B)) ChessBlack,
      Square (Just (Piece ChessWhite N)) ChessWhite,
      Square (Just (Piece ChessWhite R)) ChessBlack
    ]
  ]

validMove1 :: Board
validMove1 = [
    [ 
      -- Start
      Square Nothing ChessBlack,
      Square (Just (Piece ChessBlack N)) ChessWhite,
      Square (Just (Piece ChessBlack B)) ChessBlack,
      Square (Just (Piece ChessBlack Q)) ChessWhite,
      Square (Just (Piece ChessBlack K)) ChessBlack,
      Square (Just (Piece ChessBlack B)) ChessBlack,
      Square (Just (Piece ChessBlack N)) ChessWhite,
      Square (Just (Piece ChessBlack R)) ChessBlack
    ],
    replicate 8 $ Square (Just (Piece ChessBlack P)) ChessWhite,
    replicate 8 $ Square Nothing ChessWhite,
    replicate 8 $ Square Nothing ChessWhite,
    replicate 8 $ Square Nothing ChessWhite,
    replicate 8 $ Square Nothing ChessWhite,
    replicate 8 $ Square (Just (Piece ChessWhite P)) ChessWhite,
    [ 
      Square (Just (Piece ChessWhite R)) ChessBlack,
      Square (Just (Piece ChessWhite N)) ChessWhite,
      Square (Just (Piece ChessWhite B)) ChessBlack,
      Square (Just (Piece ChessWhite Q)) ChessWhite,
      Square (Just (Piece ChessWhite K)) ChessBlack,
      Square (Just (Piece ChessWhite B)) ChessBlack,
      Square (Just (Piece ChessWhite N)) ChessWhite,
      --End
      Square (Just (Piece ChessBlack R)) ChessBlack
    ]
  ]

validMove2 :: Board
validMove2 = [
    [ 
      Square (Just (Piece ChessBlack R)) ChessBlack,
      Square (Just (Piece ChessBlack N)) ChessWhite,
      Square (Just (Piece ChessBlack B)) ChessBlack,
      Square (Just (Piece ChessBlack Q)) ChessWhite,
      -- End
      Square (Just (Piece ChessWhite Q)) ChessBlack,
      Square (Just (Piece ChessBlack B)) ChessBlack,
      Square (Just (Piece ChessBlack N)) ChessWhite,
      Square (Just (Piece ChessBlack R)) ChessBlack
    ],
    replicate 8 $ Square (Just (Piece ChessBlack P)) ChessWhite,
    replicate 8 $ Square Nothing ChessWhite,
    replicate 8 $ Square Nothing ChessWhite,
    replicate 8 $ Square Nothing ChessWhite,
    replicate 8 $ Square Nothing ChessWhite,
    replicate 8 $ Square (Just (Piece ChessWhite P)) ChessWhite,
    [ 
      Square (Just (Piece ChessWhite R)) ChessBlack,
      Square (Just (Piece ChessWhite N)) ChessWhite,
      Square (Just (Piece ChessWhite B)) ChessBlack,
      -- Start
      Square Nothing ChessWhite,
      Square (Just (Piece ChessWhite K)) ChessBlack,
      Square (Just (Piece ChessWhite B)) ChessBlack,
      Square (Just (Piece ChessWhite N)) ChessWhite,
      Square (Just (Piece ChessWhite R)) ChessBlack
    ]
  ]