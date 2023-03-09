module Spec where

import State
import Test.HUnit

main :: IO ()
main = putStrLn "Test suite not yet implemented"

-- testParseWeather :: Test
-- > testParseWeather = "testParseWeather" ~:
-- >       TestList [ parseWeather (Map.fromList
-- >                                [("day", "2"), ("maxTemp", "78"), ("minTemp", "62")])
-- >                  ~?= Just example,
-- >                  parseWeather (Map.fromList [("day", "2")]) ~?= Nothing,
-- >                  parseWeather (Map.fromList
-- >                               [("day", "two"), ("maxTemp", "78"), ("minTemp", "62")])
-- >                  ~?= Nothing ]

testState :: Board
testState = [[]]

-- move
testMove :: Test
testMove = "testMove" ~:
  TestList [
    apply move 
  ]
