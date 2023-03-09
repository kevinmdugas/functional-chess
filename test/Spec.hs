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
    apply move ((0,0), (0,0)) initTestBoard ~?= (Nothing, initTestBoard),
    apply move ((8,0), (0,0)) initTestBoard ~?= (Nothing, initTestBoard),
    apply move ((0,8), (0,0)) initTestBoard ~?= (Nothing, initTestBoard), 
    apply move ((0,0), (-1,0)) initTestBoard ~?= (Nothing, initTestBoard), 
    apply move ((0,0), (0,-1)) initTestBoard ~?= (Nothing, initTestBoard), 

    -- Valid
    apply move ((1,0), (4,1)) initTestBoard ~?= (
                                                  Just (Piece ChessWhite P),
                                                  testMoveBoard1
                                                ) 
  ]
