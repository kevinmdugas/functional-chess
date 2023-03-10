module Main (main) where

import qualified Text.Read as Text
  
import Board
import State
import Interface

main :: IO ()
main = do
  mapM_ print menu
  choice <- getLine
  if choice == "1" then play (Nothing, startState)
  else if choice == "2" then review
  else if choice == "3" then return ()
  else do
    putStrLn "Invalid input"
    main

play :: (Maybe Piece, GameState) -> IO ()
play (p, state) = do
  printBoard $ updateBoard emptyBoard state
  putStrLn "curr: "
  start <- getLine
  putStrLn "dest: "
  end <- getLine
  play $ apply move (getPositions start end) state

getPositions :: String -> String -> (Pos, Pos)
getPositions cur dest = (Text.read cur :: Pos, Text.read dest :: Pos)

review :: IO ()
review = undefined