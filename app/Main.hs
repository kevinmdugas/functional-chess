module Main (main) where
  
import Board
import Piece
import Interface

main :: IO ()
main = do
  mapM_ print menu
  choice <- getLine
  if choice == "1" then play initBoard
  else if choice == "2" then review
  else if choice == "3" then return ()
  else do
    putStrLn "Invalid input"
    main

play :: Board -> IO ()
play board = do
  printBoard board
  response <- getLine
  if endMatch response then main
  else play board

endMatch :: String -> Bool
endMatch s = s == "quit"

review :: IO ()
review = undefined