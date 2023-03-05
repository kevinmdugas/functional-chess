module Main (main) where
  
import Board
import Interface

main :: IO ()
main = do
  mapM_ print menu
  choice <- getLine
  if choice == "1" then play
  else if choice == "2" then review
  else do
    putStrLn "Invalid input"
    main

play :: IO ()
play = do
  printBoard $ updateBoard emptyBoard startState
  attemptedMove <- getLine
  putStrLn attemptedMove

review :: IO ()
review = undefined