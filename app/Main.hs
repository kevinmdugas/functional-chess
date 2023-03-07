module Main (main) where
  
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
  response <- getLine
  if endMatch response then main
  else
    play $ apply move ((0,0), Nothing) board

endMatch :: String -> Bool
endMatch s = s == "quit"

review :: IO ()
review = undefined