module Spec where

import State
import Piece
import Board
import TestData

import Test.HUnit

main :: IO ()
main = do
  _ <- runTestTT $ TestList [ testMove ]
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

    -- -- Valid
    apply move ((0,0), (7,7)) startState ~?= (Just (Piece ChessWhite R), validMove1),
    apply move ((7,3), (0,4)) startState ~?= (Just (Piece ChessBlack K), validMove2), 
    apply move ((0,4), (7,3)) validMove2 ~?= (Nothing, validMove3) 
  ]
