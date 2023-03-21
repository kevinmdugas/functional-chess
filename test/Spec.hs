module Spec (main) where

import State
import Board
import LANParser
import Validate
import TestData

import Test.HUnit

main :: IO ()
main = do
  _ <- runTestTT $ TestList [
    testMove,
    testParse,
    testCastleValidate,
    testPawnValidate,
    testRookValidate,
    testBishopValidate,
    testQueenValidate,
    testKnightValidate,
    testKingValidate]
  return ()
  
-- The testMove data uses a board of dimensions 5 x 2 to be able to statically
-- define multiple instances of expected board outputs with less repetitive code.
testMove :: Test
testMove = "testMove" ~:
  TestList [
    -- Invalid
    apply move ((0,0), (0,0)) startState ~?= (Nothing, startState),
    apply move ((8,0), (0,0)) startState ~?= (Nothing, startState),
    apply move ((0,8), (0,0)) startState ~?= (Nothing, startState), 
    apply move ((0,0), (-1,0)) startState ~?= (Nothing, startState), 
    apply move ((0,0), (0,-1)) startState ~?= (Nothing, startState), 

    -- Valid
    apply move ((0,0), (7,7)) startState ~?= (Just (Piece ChessWhite R False), validMove1),
    apply move ((7,3), (0,4)) startState ~?= (Just (Piece ChessBlack K False), validMove2), 
    apply move ((0,4), (7,3)) validMove2 ~?= (Nothing, validMove3),
    apply move ((0,0), (0,1)) startState ~?= (Just (Piece ChessBlack N False), validMove4),
    apply move ((0,1), (0,0)) startState ~?= (Just (Piece ChessBlack R False), validMove5)
  ]

testParse :: Test
testParse = "testParse" ~:
  TestList [
    -- Invalid
    parseMove "" ChessWhite ~?= (Nothing, Nothing),
    parseMove "" ChessBlack ~?= (Nothing, Nothing),
    parseMove "Za1-a2" ChessBlack ~?= (Nothing, Nothing),
    parseMove "Kz1-a2" ChessBlack ~?= (Nothing, Nothing),
    parseMove "Ka9-a2" ChessBlack ~?= (Nothing, Nothing),
    parseMove "Ka1-z2" ChessBlack ~?= (Nothing, Nothing),
    parseMove "Ka1-a9" ChessBlack ~?= (Nothing, Nothing),
    parseMove "Ka1a2" ChessBlack ~?= (Nothing, Nothing),
    parseMove "Ka1+a2" ChessBlack ~?= (Nothing, Nothing),
    parseMove "Ka1-a2z" ChessBlack ~?= (Nothing, Nothing),

    -- Valid
      -- Castling
    parseMove "O-O" ChessWhite ~?= 
      (Just (Piece { color = ChessWhite, ptype = K, moved = False }, (7, 4), (7, 6)), 
       Just (Piece { color = ChessWhite, ptype = R, moved = False }, (7, 7), (7, 5))),
    parseMove "0-0" ChessWhite ~?= 
      (Just (Piece { color = ChessWhite, ptype = K, moved = False }, (7, 4), (7, 6)), 
       Just (Piece { color = ChessWhite, ptype = R, moved = False }, (7, 7), (7, 5))),
    parseMove "O-O-O" ChessWhite ~?= 
      (Just (Piece { color = ChessWhite, ptype = K, moved = False }, (7, 4), (7, 2)), 
       Just (Piece { color = ChessWhite, ptype = R, moved = False }, (7, 0), (7, 3))),
    parseMove "0-0-0" ChessWhite ~?= 
      (Just (Piece { color = ChessWhite, ptype = K, moved = False }, (7, 4), (7, 2)), 
       Just (Piece { color = ChessWhite, ptype = R, moved = False }, (7, 0), (7, 3))),

    parseMove "O-O" ChessBlack ~?= 
      (Just (Piece { color = ChessBlack, ptype = K, moved = False }, (0, 4), (0, 6)), 
       Just (Piece { color = ChessBlack, ptype = R, moved = False }, (0, 7), (0, 5))),
    parseMove "0-0" ChessBlack ~?= 
      (Just (Piece { color = ChessBlack, ptype = K, moved = False }, (0, 4), (0, 6)), 
       Just (Piece { color = ChessBlack, ptype = R, moved = False }, (0, 7), (0, 5))),
    parseMove "O-O-O" ChessBlack ~?= 
      (Just (Piece { color = ChessBlack, ptype = K, moved = False }, (0, 4), (0, 2)), 
       Just (Piece { color = ChessBlack, ptype = R, moved = False }, (0, 0), (0, 3))),
    parseMove "0-0-0" ChessBlack ~?= 
      (Just (Piece { color = ChessBlack, ptype = K, moved = False }, (0, 4), (0, 2)), 
       Just (Piece { color = ChessBlack, ptype = R, moved = False }, (0, 0), (0, 3))),

      -- Other Moves
    parseMove "Ka1-a2" ChessBlack ~?= 
      (Just (Piece { color = ChessBlack, ptype = K, moved = False }, (7, 0), (6, 0)), Nothing),
    parseMove "Kb1-a7" ChessWhite ~?= 
      (Just (Piece { color = ChessWhite, ptype = K, moved = False }, (7, 1), (1, 0)), Nothing),
    parseMove "Re6-c6" ChessBlack ~?= 
      (Just (Piece { color = ChessBlack, ptype = R, moved = False }, (2, 4), (2, 2)), Nothing),
    parseMove "Nf4-g2" ChessWhite ~?= 
      (Just (Piece { color = ChessWhite, ptype = N, moved = False }, (4, 5), (6, 6)), Nothing),
    parseMove "Bh3-d7" ChessBlack ~?= 
      (Just (Piece { color = ChessBlack, ptype = B, moved = False }, (5, 7), (1, 3)), Nothing),
    parseMove "Qd8-g4" ChessWhite ~?= 
      (Just (Piece { color = ChessWhite, ptype = Q, moved = False }, (0, 3), (4, 6)), Nothing),
    parseMove "Ka1xa2" ChessBlack ~?= 
      (Just (Piece { color = ChessBlack, ptype = K, moved = False }, (7, 0), (6, 0)), Nothing),
    parseMove "Ka1-a2+" ChessBlack ~?= 
      (Just (Piece { color = ChessBlack, ptype = K, moved = False }, (7, 0), (6, 0)), Nothing),
    parseMove "Ka1-a2#" ChessBlack ~?= 
      (Just (Piece { color = ChessBlack, ptype = K, moved = False }, (7, 0), (6, 0)), Nothing)
  ]

testCastleValidate :: Test
testCastleValidate = "testCastleValidate" ~:
  TestList [
    -- Valid
      -- Kingside Black
    validate (
      Just (Piece {color = ChessBlack, ptype = K, moved = False}, (0,4), (0,6)),
      Just (Piece {color = ChessBlack, ptype = R, moved = False}, (0,7), (0,5))
    ) ChessBlack validCastle ~?= True,
      -- Kingside White
    validate (
      Just (Piece {color = ChessWhite, ptype = K, moved = False}, (7,4), (7,6)),
      Just (Piece {color = ChessWhite, ptype = R, moved = False}, (7,7), (7,5))
    ) ChessWhite validCastle ~?= True,
      -- Queenside Black
    validate (
      Just (Piece {color = ChessBlack, ptype = K, moved = False}, (0,4), (0,2)),
      Just (Piece {color = ChessBlack, ptype = R, moved = False}, (0,0), (0,3))
    ) ChessBlack validCastle ~?= True,
      -- Queenside White
    validate (
      Just (Piece {color = ChessWhite, ptype = K, moved = False}, (7,4), (7,2)),
      Just (Piece {color = ChessWhite, ptype = R, moved = False}, (7,0), (7,3))
    ) ChessWhite validCastle ~?= True,

    -- Invalid
      -- Wrong color
    validate (
      Just (Piece {color = ChessWhite, ptype = K, moved = False}, (7,4), (7,2)),
      Just (Piece {color = ChessWhite, ptype = R, moved = False}, (7,0), (7,3))
    ) ChessBlack validCastle ~?= False,
      -- Space occupied
    validate (
      Just (Piece {color = ChessBlack, ptype = K, moved = False}, (0,4), (0,6)),
      Just (Piece {color = ChessBlack, ptype = R, moved = False}, (0,7), (0,5))
    ) ChessBlack startState ~?= False,
      -- Not first move for king
    validate (
      Just (Piece {color = ChessBlack, ptype = K, moved = False}, (0,4), (0,2)),
      Just (Piece {color = ChessBlack, ptype = R, moved = False}, (0,0), (0,3))
    ) ChessBlack invalidCastle1 ~?= False,
      -- Not first move for rook
    validate (
      Just (Piece {color = ChessBlack, ptype = K, moved = False}, (0,4), (0,2)),
      Just (Piece {color = ChessBlack, ptype = R, moved = False}, (0,0), (0,3))
    ) ChessBlack invalidCastle2 ~?= False,
      -- Wrong piece
    validate (
      Just (Piece {color = ChessBlack, ptype = K, moved = False}, (0,4), (0,2)),
      Just (Piece {color = ChessBlack, ptype = R, moved = False}, (0,0), (0,3))
    ) ChessBlack invalidCastle3 ~?= False,
      -- No rook present
    validate (
      Just (Piece {color = ChessBlack, ptype = K, moved = False}, (0,4), (0,2)),
      Just (Piece {color = ChessBlack, ptype = R, moved = False}, (0,0), (0,3))
    ) ChessBlack invalidCastle4 ~?= False
  ]

testPawnValidate :: Test
testPawnValidate = "testPawnValidate" ~:
  TestList [
    -- Move pawn: Valid
      -- Move two spaces on first turn
    validate (
      Just (Piece {color = ChessBlack, ptype = P, moved = False}, (1,1), (3,1)),
      Nothing
    ) ChessBlack startState ~?= True,
      -- Move single space in black direction
    validate (
      Just (Piece {color = ChessBlack, ptype = P, moved = False}, (1,1), (2,1)),
      Nothing
    ) ChessBlack startState ~?= True,
      -- Move single space in white direction
    validate (
      Just (Piece {color = ChessWhite, ptype = P, moved = False}, (6,1), (5,1)),
      Nothing
    ) ChessWhite startState ~?= True,
      -- Move diagonally to capture piece: black
    validate (
      Just (Piece {color = ChessBlack, ptype = P, moved = False}, (0,1), (1,0)),
      Nothing
    ) ChessBlack validPawn ~?= True,
      -- Move diagonally to capture piece: white
    validate (
      Just (Piece {color = ChessWhite, ptype = P, moved = False}, (1,0), (0,1)),
      Nothing
    ) ChessWhite validPawn ~?= True,
    
    -- Move pawn: Invalid
      -- Empty space
    validate (
      Just (Piece {color = ChessWhite, ptype = P, moved = False}, (1,2), (0,1)),
      Nothing
    ) ChessWhite validPawn ~?= False,
      -- Wrong piece
    validate (
      Just (Piece {color = ChessWhite, ptype = P, moved = False}, (0,1), (1,1)),
      Nothing
    ) ChessWhite validPawn ~?= False,
      -- Move wrong direction for color
    validate (
      Just (Piece {color = ChessWhite, ptype = P, moved = False}, (1,0), (2,0)),
      Nothing
    ) ChessWhite validPawn ~?= False,
      -- Move anywhere but one space ahead, two ahead, or diagonally
    validate (
      Just (Piece {color = ChessWhite, ptype = P, moved = False}, (1,0), (1,1)),
      Nothing
    ) ChessWhite validPawn ~?= False,
      -- Move two spaces after first turn
    validate (
      Just (Piece {color = ChessWhite, ptype = P, moved = False}, (2,2), (1,0)),
      Nothing
    ) ChessWhite validPawn ~?= False,
      -- Move forward one but space is occupied
    validate (
      Just (Piece {color = ChessBlack, ptype = P, moved = False}, (3,2), (4,2)),
      Nothing
    ) ChessBlack validPawn ~?= False,
      -- Move diagonally but no opponent piece present
    validate (
      Just (Piece {color = ChessWhite, ptype = P, moved = False}, (4,2), (3,1)),
      Nothing
    ) ChessWhite validPawn ~?= False
  ]

testRookValidate :: Test
testRookValidate = "testRookValidate" ~:
  TestList [
    -- Valid
      -- Horizontal left
    validate (
      Just (Piece {color = ChessBlack, ptype = R, moved = False}, (0,1), (0,0)),
      Nothing
    ) ChessBlack validRook ~?= True,
      -- Horizontal right
    validate (
      Just (Piece {color = ChessBlack, ptype = R, moved = False}, (0,1), (0,2)),
      Nothing
    ) ChessBlack validRook ~?= True,
      -- Vertical up
    validate (
      Just (Piece {color = ChessWhite, ptype = R, moved = False}, (1,0), (4,0)),
      Nothing
    ) ChessWhite validRook ~?= True,
      -- Vertical down
    validate (
      Just (Piece {color = ChessWhite, ptype = R, moved = False}, (1,0), (0,0)),
      Nothing
    ) ChessWhite validRook ~?= True,
      -- Vertical (descending) clear path and capture
    validate (
      Just (Piece {color = ChessWhite, ptype = R, moved = False}, (4,1), (2,1)),
      Nothing
    ) ChessWhite validRook ~?= True,
      -- Vertical (ascending) clear path and capture
    validate (
      Just (Piece {color = ChessBlack, ptype = R, moved = False}, (2,1), (4,1)),
      Nothing
    ) ChessBlack validRook ~?= True,
      -- Horizontal (left to right) clear path and capture
    validate (
      Just (Piece {color = ChessBlack, ptype = R, moved = False}, (5,0), (5,2)),
      Nothing
    ) ChessBlack validRook ~?= True,
      -- Horizontal (right to left) clear path and capture
    validate (
      Just (Piece {color = ChessWhite, ptype = R, moved = False}, (5,2), (5,0)),
      Nothing
    ) ChessWhite validRook ~?= True,

    -- Invalid
      -- Horizontal space taken
    validate (
      Just (Piece {color = ChessBlack, ptype = R, moved = False}, (0,0), (0,1)),
      Nothing
    ) ChessBlack startState ~?= False,
      -- Vertical space taken
    validate (
      Just (Piece {color = ChessBlack, ptype = R, moved = False}, (0,0), (1,0)),
      Nothing
    ) ChessBlack startState ~?= False,
      -- End position free but path not clear
    validate (
      Just (Piece {color = ChessBlack, ptype = R, moved = False}, (0,0), (3,0)),
      Nothing
    ) ChessBlack startState ~?= False
  ]

testBishopValidate :: Test
testBishopValidate = "testBishopValidate" ~:
  TestList [
    -- Valid
      -- Q1 Clear path and capture
    validate (
      Just (Piece {color = ChessBlack, ptype = B, moved = False}, (2,2), (0,4)),
      Nothing
    ) ChessBlack validBishop ~?= True,
      -- Q2 Clear path and capture
    validate (
      Just (Piece {color = ChessWhite, ptype = B, moved = False}, (4,4), (2,2)),
      Nothing
    ) ChessWhite validBishop ~?= True,
      -- Q2 move one space
    validate (
      Just (Piece {color = ChessWhite, ptype = B, moved = False}, (4,4), (3,3)),
      Nothing
    ) ChessWhite validBishop ~?= True,
      -- Q3 Clear path and capture; one space
    validate (
      Just (Piece {color = ChessBlack, ptype = B, moved = False}, (2,2), (3,1)),
      Nothing
    ) ChessBlack validBishop ~?= True,
      -- Q4 Clear path and capture; one space
    validate (
      Just (Piece {color = ChessBlack, ptype = B, moved = False}, (2,2), (4,4)),
      Nothing
    ) ChessBlack validBishop ~?= True,

    -- Invalid
      -- Q1 blocked path
    validate (
      Just (Piece {color = ChessBlack, ptype = B, moved = False}, (3,0), (0,3)),
      Nothing
    ) ChessBlack validBishop ~?= False,
      -- Q1 not in vector path
    validate (
      Just (Piece {color = ChessBlack, ptype = B, moved = False}, (2,2), (1,4)),
      Nothing
    ) ChessBlack validBishop ~?= False,
      -- Q2 blocked path
    validate (
      Just (Piece {color = ChessWhite, ptype = B, moved = False}, (4,4), (0,0)),
      Nothing
    ) ChessWhite validBishop ~?= False,
      -- Q2 not in vector path
    validate (
      Just (Piece {color = ChessWhite, ptype = B, moved = False}, (4,4), (2,1)),
      Nothing
    ) ChessWhite validBishop ~?= False,
      -- Q3 blocked path
    validate (
      Just (Piece {color = ChessBlack, ptype = B, moved = False}, (2,2), (4,0)),
      Nothing
    ) ChessBlack validBishop ~?= False,
      -- Q3 not in vector path
    validate (
      Just (Piece {color = ChessWhite, ptype = B, moved = False}, (1,2), (2,0)),
      Nothing
    ) ChessWhite validBishop ~?= False,
      -- Q4 blocked path
    validate (
      Just (Piece {color = ChessWhite, ptype = B, moved = False}, (0,1), (3,4)),
      Nothing
    ) ChessWhite validBishop ~?= False,
      -- Q4 not in vector path
    validate (
      Just (Piece {color = ChessBlack, ptype = B, moved = False}, (2,2), (4,3)),
      Nothing
    ) ChessBlack validBishop ~?= False
  ]

testQueenValidate :: Test
testQueenValidate = "testQueenValidate" ~:
  TestList [
    -- Valid
      -- Horizontal capture
    validate (
      Just (Piece {color = ChessBlack, ptype = Q, moved = False}, (2,2), (2,4)),
      Nothing
    ) ChessBlack validQueen ~?= True,
      -- Vertical capture
    validate (
      Just (Piece {color = ChessBlack, ptype = Q, moved = False}, (2,2), (3,2)),
      Nothing
    ) ChessBlack validQueen ~?= True,
      -- Diagonal capture
    validate (
      Just (Piece {color = ChessBlack, ptype = Q, moved = False}, (2,2), (0,4)),
      Nothing
    ) ChessBlack validQueen ~?= True,

    -- Invalid
      -- Horizontal wrong color
    validate (
      Just (Piece {color = ChessWhite, ptype = Q, moved = False}, (1,4), (1,2)),
      Nothing
    ) ChessWhite validQueen ~?= False
    
  ]

testKnightValidate :: Test
testKnightValidate = "testKnightValidate" ~:
  TestList [
    -- Valid
      -- Capture
    validate (
      Just (Piece {color = ChessBlack, ptype = N, moved = False}, (2,2), (0,1)),
      Nothing
    ) ChessBlack validKnight ~?= True,
    validate (
      Just (Piece {color = ChessBlack, ptype = N, moved = False}, (2,2), (1,0)),
      Nothing
    ) ChessBlack validKnight ~?= True,

    -- Invalid
      -- Wrong color
    validate (
      Just (Piece {color = ChessBlack, ptype = N, moved = False}, (2,2), (1,4)),
      Nothing
    ) ChessBlack validKnight ~?= False,
      -- Empty space
    validate (
      Just (Piece {color = ChessBlack, ptype = N, moved = False}, (2,2), (1,1)),
      Nothing
    ) ChessBlack validKnight ~?= False
  ]

testKingValidate :: Test
testKingValidate = "testKingValidate" ~:
  TestList [
    -- Valid 
    validate (
      Just (Piece {color = ChessBlack, ptype = K, moved = False}, (2,2), (1,2)),
      Nothing
    ) ChessBlack validKing ~?= True,
    validate (
      Just (Piece {color = ChessBlack, ptype = K, moved = False}, (2,2), (2,1)),
      Nothing
    ) ChessBlack validKing ~?= True,

    -- Invalid
    validate (
      Just (Piece {color = ChessBlack, ptype = K, moved = False}, (2,2), (2,3)),
      Nothing
    ) ChessBlack validKing ~?= False,
    validate (
      Just (Piece {color = ChessBlack, ptype = K, moved = False}, (2,2), (0,3)),
      Nothing
    ) ChessBlack validKing ~?= False
  ]