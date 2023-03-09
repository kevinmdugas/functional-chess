module Main (main) where

import qualified Text.Read as Text
  
import Board
import Piece
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

play :: (Maybe Piece, Board) -> IO ()
play (p, board) = do
  printBoard board
  putStrLn "curr: "
  cur <- getLine
  putStrLn "dest: "
  dest <- getLine
  play $ apply move (getPositions cur dest) board

getPositions :: String -> String -> (Pos, Pos)
getPositions cur dest = (Text.read cur :: Pos, Text.read dest :: Pos)

endMatch :: String -> Bool
endMatch s = s == "quit"

review :: IO ()
review = undefined