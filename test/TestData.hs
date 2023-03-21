module TestData where

import Board

validMove1 :: GameState
validMove1 = [
  [ Nothing, Just (Piece ChessBlack N False), 
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
    Just (Piece ChessWhite N False), Just (Piece ChessBlack R True)]]

validMove2 :: GameState
validMove2 = [
  [ Just (Piece ChessBlack R False), Just (Piece ChessBlack N False), 
    Just (Piece ChessBlack B False), Just (Piece ChessBlack Q False), 
    Just (Piece ChessWhite Q True), Just (Piece ChessBlack B False), 
    Just (Piece ChessBlack N False), Just (Piece ChessBlack R False) ], 
  replicate 8 (Just (Piece ChessBlack P False)),
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 (Just (Piece ChessWhite P False)),
  [ Just (Piece ChessWhite R False), Just (Piece ChessWhite N False), 
    Just (Piece ChessWhite B False), Nothing, 
    Just (Piece ChessWhite K False), Just (Piece ChessWhite B False), 
    Just (Piece ChessWhite N False), Just (Piece ChessWhite R False) ]]

validMove3 :: GameState
validMove3 = [
  [ Just (Piece ChessBlack R False), Just (Piece ChessBlack N False), 
    Just (Piece ChessBlack B False), Just (Piece ChessBlack Q False), 
    Nothing, Just (Piece ChessBlack B False), 
    Just (Piece ChessBlack N False), Just (Piece ChessBlack R False) ], 
  replicate 8 (Just (Piece ChessBlack P False)),
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 (Just (Piece ChessWhite P False)),
  [ Just (Piece ChessWhite R False), Just (Piece ChessWhite N False), 
    Just (Piece ChessWhite B False), Just (Piece ChessWhite Q True), 
    Just (Piece ChessWhite K False), Just (Piece ChessWhite B False), 
    Just (Piece ChessWhite N False), Just (Piece ChessWhite R False) ]]

validMove4 :: [[Maybe Piece]]
validMove4 = [
  [ Nothing, Just (Piece ChessBlack R True), 
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

validMove5 :: [[Maybe Piece]]
validMove5 = [
  [ Just (Piece ChessBlack N True), Nothing, 
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

validCastle :: GameState
validCastle = [
  [ Just (Piece ChessBlack R False), Just (Piece ChessBlack N False), 
    Nothing, Nothing, 
    Just (Piece ChessBlack K False), Just (Piece ChessWhite B False), 
    Nothing, Just (Piece ChessBlack R False) ], 
  replicate 8 (Just (Piece ChessBlack P False)),
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 (Just (Piece ChessWhite P False)),
  [ Just (Piece ChessWhite R False), Just (Piece ChessWhite N False), 
    Nothing, Just (Piece ChessBlack Q False), 
    Just (Piece ChessWhite K False), Nothing, 
    Just (Piece ChessBlack N False), Just (Piece ChessWhite R False) ]]
  
invalidCastle1 :: GameState
invalidCastle1 = [
  [ Just (Piece ChessBlack R False), Just (Piece ChessBlack N False), 
    Nothing, Nothing, 
    Just (Piece ChessBlack K True), Just (Piece ChessWhite B False), 
    Nothing, Just (Piece ChessBlack R False) ], 
  replicate 8 (Just (Piece ChessBlack P False)),
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 (Just (Piece ChessWhite P False)),
  [ Just (Piece ChessWhite R False), Just (Piece ChessWhite N False), 
    Nothing, Just (Piece ChessBlack Q False), 
    Just (Piece ChessWhite K False), Nothing, 
    Just (Piece ChessBlack N False), Just (Piece ChessWhite R False) ]]

invalidCastle2 :: GameState
invalidCastle2 = [
  [ Just (Piece ChessBlack R True), Just (Piece ChessBlack N False), 
    Nothing, Nothing, 
    Just (Piece ChessBlack K False), Just (Piece ChessWhite B False), 
    Nothing, Just (Piece ChessBlack R False) ], 
  replicate 8 (Just (Piece ChessBlack P False)),
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 (Just (Piece ChessWhite P False)),
  [ Just (Piece ChessWhite R False), Just (Piece ChessWhite N False), 
    Nothing, Just (Piece ChessBlack Q False), 
    Just (Piece ChessWhite K False), Nothing, 
    Just (Piece ChessBlack N False), Just (Piece ChessWhite R False) ]]

invalidCastle3 :: GameState
invalidCastle3 = [
  [ Just (Piece ChessBlack B True), Just (Piece ChessBlack N False), 
    Nothing, Nothing, 
    Just (Piece ChessBlack K False), Just (Piece ChessWhite B False), 
    Nothing, Just (Piece ChessBlack R False) ], 
  replicate 8 (Just (Piece ChessBlack P False)),
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 (Just (Piece ChessWhite P False)),
  [ Just (Piece ChessWhite R False), Just (Piece ChessWhite N False), 
    Nothing, Just (Piece ChessBlack Q False), 
    Just (Piece ChessWhite K False), Nothing, 
    Just (Piece ChessBlack N False), Just (Piece ChessWhite R False) ]]

invalidCastle4 :: GameState
invalidCastle4 = [
  [ Nothing, Just (Piece ChessBlack N False), 
    Nothing, Nothing, 
    Just (Piece ChessBlack K False), Just (Piece ChessWhite B False), 
    Nothing, Just (Piece ChessBlack R False) ], 
  replicate 8 (Just (Piece ChessBlack P False)),
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 (Just (Piece ChessWhite P False)),
  [ Just (Piece ChessWhite R False), Just (Piece ChessWhite N False), 
    Nothing, Just (Piece ChessBlack Q False), 
    Just (Piece ChessWhite K False), Nothing, 
    Just (Piece ChessBlack N False), Just (Piece ChessWhite R False) ]]

validPawn :: GameState
validPawn = [
  [ Nothing, Just (Piece ChessBlack P False), Nothing ],
  [ Just (Piece ChessWhite P False), Nothing, Nothing ],
  [ Nothing, Nothing, Just (Piece ChessWhite P True) ],
  [ Nothing, Nothing, Just (Piece ChessBlack P True) ],
  [ Nothing, Nothing, Just (Piece ChessWhite P True) ]]

validRook :: GameState
validRook = [
  [ Nothing, Just (Piece ChessBlack R False), Nothing ],
  [ Just (Piece ChessWhite R False), Nothing, Nothing ],
  [ Nothing, Just (Piece ChessBlack R False), Just (Piece ChessWhite P True) ],
  [ Nothing, Nothing, Nothing ],
  [ Nothing, Just (Piece ChessWhite R True), Just (Piece ChessWhite R True) ],
  [ Just (Piece ChessBlack R False), Nothing, Just (Piece ChessWhite R True) ]]

wB_____ :: Maybe Piece
wB_____ = Just (Piece ChessWhite B False)

bB_____ :: Maybe Piece
bB_____ = Just (Piece ChessBlack B False)

validBishop :: GameState
validBishop = [
  [ Nothing, wB_____, Nothing, wB_____, wB_____ ],
  [ Nothing, Nothing, wB_____, Nothing, wB_____ ],
  [ Nothing, Nothing, bB_____, Nothing, Nothing ],
  [ bB_____, wB_____, Nothing, Nothing, Nothing ],
  [ Nothing, Nothing, Nothing, Nothing, wB_____ ]]

wQ_____ :: Maybe Piece
wQ_____ = Just (Piece ChessWhite Q False)

bQ_____ :: Maybe Piece
bQ_____ = Just (Piece ChessBlack Q False)

validQueen :: GameState
validQueen = [
  [ Nothing, wB_____, Nothing, wB_____, wB_____ ],
  [ Nothing, Nothing, wB_____, Nothing, wB_____ ],
  [ Nothing, Nothing, bQ_____, Nothing, wQ_____ ],
  [ bB_____, wB_____, Nothing, Nothing, Nothing ],
  [ Nothing, Nothing, bQ_____, Nothing, wQ_____ ]]