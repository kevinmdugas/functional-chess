module Main (main) where
  
import System.Directory (doesFileExist)

import Board
import State
import Interface
import LANParser
import GameParser
import Validate

main :: IO ()
main = do
  mapM_ putStrLn menu
  choice <- getLine
  if choice == "1" then play (([], []), startState, ChessWhite, ((8,8),(8,8)))
  else if choice == "2" then review
  else if choice == "3" then return ()
  else do
    putStrLn "Invalid input"
    main

play :: (Captures, GameState, ChessColor, (Pos, Pos)) -> IO ()
play (caps, state, player, lastMove) = do
  display (updateBoard emptyBoard state) caps player lastMove
  putStrLn "Enter move (using long algebraic notation):"
  moveStr <- getLine
  if moveStr == "quit" then main
  else do
    let nextPlayer = oppColor player
    let moveSet = parseMove moveStr player
    if validate moveSet player state then do
      let (captP, newState, newMove) = makeMove moveSet state
      if determineWinner captP then mapM_ putStrLn $ winScreen player
      else
        play (addCapture captP caps player, newState, nextPlayer, newMove)
    else do
      putStrLn "Invalid Move" >> play (caps, state, player, lastMove)

determineWinner :: Maybe Piece -> Bool
determineWinner Nothing = False
determineWinner (Just p) = ptype p == K 

review :: IO ()
review = do
  mapM_ putStrLn reviewMenu
  putStrLn "Enter path to game file: "
  path <- getLine
  fileExists <- doesFileExist path
  if not fileExists
    then putStrLn "Error: file not found" >> review
  else do
    contents <- readFile path
    let moves = parseGameFile contents
        result = getGameResult $ words contents
    stepLoop moves 0 (([], []), startState, ChessWhite, ((8,8),(8,8)), result)

stepLoop :: [(Maybe ChessMove, Maybe ChessMove)] -> Int 
  -> (Captures, GameState, ChessColor, (Pos, Pos), String) -> IO ()
stepLoop moveList n (caps, state, player, lastMove, res) = do
  printBoard (updateBoard emptyBoard state) player lastMove
  if n == length moveList - 1 then putStrLn $ reviewResult res
  else putStr ""
  putStrLn "Options: >, <, flip, quit"
  choice <- getLine
  case choice of
    ">"    -> 
      if n + 1 >= length moveList
        then putStrLn "Invalid Move" >> 
             stepLoop moveList n (caps, state, player, lastMove, res)
        else case makeMove (moveList !! n) state of
          (newP, newState, newMove) ->
            stepLoop moveList (n + 1) (addCapture newP caps player, newState, player, newMove, res)
    "<"    -> 
      if n - 1 < 0
        then putStrLn "Invalid Move" >> 
             stepLoop moveList n (caps, state, player, lastMove, res)
        else case makeMove (reverseMove (moveList !! (n - 1))) state of 
          (_, newState, (x, y)) -> do
            let (newCaps, p) = removeCapture caps ChessWhite -- one list is sufficient here
            stepLoop moveList (n - 1) (newCaps, placePiece p x newState, player, (x, y), res)
    "flip" -> stepLoop moveList n (caps, state, oppColor player, lastMove, res)
    "quit" -> main
    _      -> putStrLn "Invalid option" >> 
              stepLoop moveList n (caps, state, player, lastMove, res)