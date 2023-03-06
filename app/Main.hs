module Main (main) where
  
import Board
import Piece
import Interface

main :: IO ()
main = do
  mapM_ print menu
  choice <- getLine
  if choice == "1" then play emptyBoard startState
  else if choice == "2" then review
  else if choice == "3" then return ()
  else do
    putStrLn "Invalid input"
    main

play :: Board -> [[Maybe Piece]] -> IO ()
play cur new = do
  printBoard $ updateBoard cur new
  response <- getLine
  if endMatch response then main
  else play cur new

endMatch :: String -> Bool
endMatch s = s == "quit"

review :: IO ()
review = undefined