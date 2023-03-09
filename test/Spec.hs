module Spec where

import State
import Piece
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
    apply move ((0,0), (0,0)) testBoard ~?= (Nothing, testBoard),
    apply move ((8,0), (0,0)) testBoard ~?= (Nothing, testBoard),
    apply move ((0,8), (0,0)) testBoard ~?= (Nothing, testBoard), 
    apply move ((0,0), (-1,0)) testBoard ~?= (Nothing, testBoard), 
    apply move ((0,0), (0,-1)) testBoard ~?= (Nothing, testBoard), 

    -- Valid
    apply move ((0,0), (7,7)) testBoard ~?= (Just (Piece ChessWhite R), validMove1),
    apply move ((7,3), (0,4)) testBoard ~?= (Just (Piece ChessBlack K), validMove2) 
  ]
