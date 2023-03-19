module Spec (main) where

import State
import Board
import LANParser
import Validate
import TestData

import Test.HUnit

main :: IO ()
main = do
  _ <- runTestTT $ TestList [ testMove, testParse, testValidate ]
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
      (Just (Just Piece { color = ChessWhite, ptype = K, moved = False }, (7, 4), (7, 6)), 
       Just (Just Piece { color = ChessWhite, ptype = R, moved = False }, (7, 7), (7, 5))),
    parseMove "0-0" ChessWhite ~?= 
      (Just (Just Piece { color = ChessWhite, ptype = K, moved = False }, (7, 4), (7, 6)), 
       Just (Just Piece { color = ChessWhite, ptype = R, moved = False }, (7, 7), (7, 5))),
    parseMove "O-O-O" ChessWhite ~?= 
      (Just (Just Piece { color = ChessWhite, ptype = K, moved = False }, (7, 4), (7, 2)), 
       Just (Just Piece { color = ChessWhite, ptype = R, moved = False }, (7, 0), (7, 3))),
    parseMove "0-0-0" ChessWhite ~?= 
      (Just (Just Piece { color = ChessWhite, ptype = K, moved = False }, (7, 4), (7, 2)), 
       Just (Just Piece { color = ChessWhite, ptype = R, moved = False }, (7, 0), (7, 3))),

    parseMove "O-O" ChessBlack ~?= 
      (Just (Just Piece { color = ChessBlack, ptype = K, moved = False }, (0, 4), (0, 6)), 
       Just (Just Piece { color = ChessBlack, ptype = R, moved = False }, (0, 7), (0, 5))),
    parseMove "0-0" ChessBlack ~?= 
      (Just (Just Piece { color = ChessBlack, ptype = K, moved = False }, (0, 4), (0, 6)), 
       Just (Just Piece { color = ChessBlack, ptype = R, moved = False }, (0, 7), (0, 5))),
    parseMove "O-O-O" ChessBlack ~?= 
      (Just (Just Piece { color = ChessBlack, ptype = K, moved = False }, (0, 4), (0, 2)), 
       Just (Just Piece { color = ChessBlack, ptype = R, moved = False }, (0, 0), (0, 3))),
    parseMove "0-0-0" ChessBlack ~?= 
      (Just (Just Piece { color = ChessBlack, ptype = K, moved = False }, (0, 4), (0, 2)), 
       Just (Just Piece { color = ChessBlack, ptype = R, moved = False }, (0, 0), (0, 3))),

      -- Other Moves
    parseMove "Ka1-a2" ChessBlack ~?= 
      (Just (Just Piece { color = ChessBlack, ptype = K, moved = False }, (7, 0), (6, 0)), Nothing),
    parseMove "Kb1-a7" ChessWhite ~?= 
      (Just (Just Piece { color = ChessWhite, ptype = K, moved = False }, (7, 1), (1, 0)), Nothing),
    parseMove "Re6-c6" ChessBlack ~?= 
      (Just (Just Piece { color = ChessBlack, ptype = R, moved = False }, (2, 4), (2, 2)), Nothing),
    parseMove "Nf4-g2" ChessWhite ~?= 
      (Just (Just Piece { color = ChessWhite, ptype = N, moved = False }, (4, 5), (6, 6)), Nothing),
    parseMove "Bh3-d7" ChessBlack ~?= 
      (Just (Just Piece { color = ChessBlack, ptype = B, moved = False }, (5, 7), (1, 3)), Nothing),
    parseMove "Qd8-g4" ChessWhite ~?= 
      (Just (Just Piece { color = ChessWhite, ptype = Q, moved = False }, (0, 3), (4, 6)), Nothing),
    parseMove "Ka1xa2" ChessBlack ~?= 
      (Just (Just Piece { color = ChessBlack, ptype = K, moved = False }, (7, 0), (6, 0)), Nothing),
    parseMove "Ka1-a2+" ChessBlack ~?= 
      (Just (Just Piece { color = ChessBlack, ptype = K, moved = False }, (7, 0), (6, 0)), Nothing),
    parseMove "Ka1-a2#" ChessBlack ~?= 
      (Just (Just Piece { color = ChessBlack, ptype = K, moved = False }, (7, 0), (6, 0)), Nothing)
  ]

testValidate :: Test
testValidate = "testValidate" ~:
  TestList [
    -- Castling: valid
      -- Kingside Black
    validate (
      Just (Just (Piece {color = ChessBlack, ptype = K, moved = False}), (0,4), (0,6)),
      Just (Just (Piece {color = ChessBlack, ptype = R, moved = False}), (0,7), (0,5))
    ) ChessBlack validCastle ~?= True,
      -- Kingside White
    validate (
      Just (Just (Piece {color = ChessWhite, ptype = K, moved = False}), (7,4), (7,6)),
      Just (Just (Piece {color = ChessWhite, ptype = R, moved = False}), (7,7), (7,5))
    ) ChessWhite validCastle ~?= True,
      -- Queenside Black
    validate (
      Just (Just (Piece {color = ChessBlack, ptype = K, moved = False}), (0,4), (0,2)),
      Just (Just (Piece {color = ChessBlack, ptype = R, moved = False}), (0,0), (0,3))
    ) ChessBlack validCastle ~?= True,
      -- Queenside White
    validate (
      Just (Just (Piece {color = ChessWhite, ptype = K, moved = False}), (7,4), (7,2)),
      Just (Just (Piece {color = ChessWhite, ptype = R, moved = False}), (7,0), (7,3))
    ) ChessWhite validCastle ~?= True,

    -- Castling: Invalid
      -- Wrong color
    validate (
      Just (Just (Piece {color = ChessWhite, ptype = K, moved = False}), (7,4), (7,2)),
      Just (Just (Piece {color = ChessWhite, ptype = R, moved = False}), (7,0), (7,3))
    ) ChessBlack validCastle ~?= False,
      -- Space occupied
    validate (
      Just (Just (Piece {color = ChessBlack, ptype = K, moved = False}), (0,4), (0,6)),
      Just (Just (Piece {color = ChessBlack, ptype = R, moved = False}), (0,7), (0,5))
    ) ChessBlack startState ~?= False,
      -- Not first move for king
    validate (
      Just (Just (Piece {color = ChessBlack, ptype = K, moved = False}), (0,4), (0,2)),
      Just (Just (Piece {color = ChessBlack, ptype = R, moved = False}), (0,0), (0,3))
    ) ChessBlack invalidCastle1 ~?= False,
      -- Not first move for rook
    validate (
      Just (Just (Piece {color = ChessBlack, ptype = K, moved = False}), (0,4), (0,2)),
      Just (Just (Piece {color = ChessBlack, ptype = R, moved = False}), (0,0), (0,3))
    ) ChessBlack invalidCastle2 ~?= False,
      -- Wrong piece
    validate (
      Just (Just (Piece {color = ChessBlack, ptype = K, moved = False}), (0,4), (0,2)),
      Just (Just (Piece {color = ChessBlack, ptype = R, moved = False}), (0,0), (0,3))
    ) ChessBlack invalidCastle3 ~?= False,
      -- No rook present
    validate (
      Just (Just (Piece {color = ChessBlack, ptype = K, moved = False}), (0,4), (0,2)),
      Just (Just (Piece {color = ChessBlack, ptype = R, moved = False}), (0,0), (0,3))
    ) ChessBlack invalidCastle4 ~?= False,

    -- Move pawn: Valid
      -- Move two spaces on first turn
    validate (
      Just (Just (Piece {color = ChessBlack, ptype = P, moved = False}), (1,1), (3,1)),
      Nothing
    ) ChessBlack startState ~?= False,
      -- Move single space on first turn
    validate (
      Just (Just (Piece {color = ChessBlack, ptype = P, moved = False}), (1,1), (2,1)),
      Nothing
    ) ChessBlack startState ~?= False,
      -- Move single space after first turn
    validate (
      Just (Just (Piece {color = ChessBlack, ptype = P, moved = False}), (2,1), (3,1)),
      Nothing
    ) ChessBlack startState ~?= False
      -- Move diagonally to capture piece
    
    -- Move pawn: Invalid
      -- Empty space
      -- Wrong piece
      -- Move anywhere but one space ahead, two ahead, or diagonally
      -- Move two spaces after first turn
      -- Move forward one but space is occupied
      -- Move diagonally but no opponent piece present
  ]
