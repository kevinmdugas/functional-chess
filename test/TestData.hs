module TestData where

import Board
import Piece

validMove1 :: GameState
validMove1 = [
  [ Nothing, Just (Piece ChessBlack N), 
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
    Just (Piece ChessWhite N), Just (Piece ChessBlack R)]]

validMove2 :: GameState
validMove2 = [
  [ Just (Piece ChessBlack R), Just (Piece ChessBlack N), 
    Just (Piece ChessBlack B), Just (Piece ChessBlack Q), 
    Just (Piece ChessWhite Q), Just (Piece ChessBlack B), 
    Just (Piece ChessBlack N), Just (Piece ChessBlack R) ], 
  replicate 8 (Just (Piece ChessBlack P)),
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 (Just (Piece ChessWhite P)),
  [ Just (Piece ChessWhite R), Just (Piece ChessWhite N), 
    Just (Piece ChessWhite B), Nothing, 
    Just (Piece ChessWhite K), Just (Piece ChessWhite B), 
    Just (Piece ChessWhite N), Just (Piece ChessWhite R) ]]

validMove3 :: GameState
validMove3 = [
  [ Just (Piece ChessBlack R), Just (Piece ChessBlack N), 
    Just (Piece ChessBlack B), Just (Piece ChessBlack Q), 
    Nothing, Just (Piece ChessBlack B), 
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