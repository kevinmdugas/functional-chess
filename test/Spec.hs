module Spec where

import State
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
    apply move ((0,0), (0,0)) initTestBoard ~?= (Nothing, initTestBoard),
    apply move ((8,0), (0,0)) initTestBoard ~?= (Nothing, initTestBoard) 
  ]
