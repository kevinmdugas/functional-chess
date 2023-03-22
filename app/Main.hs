module Main (main) where
  
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
  moves <- parseGameFile path
  stepLoop moves 0 (([], []), startState, ChessWhite, ((8,8),(8,8)))

stepLoop :: [(Maybe ChessMove, Maybe ChessMove)] -> Int -> (Captures, GameState, ChessColor, (Pos, Pos)) -> IO ()
stepLoop moveList n (caps, state, player, lastMove) = do
  printBoard (updateBoard emptyBoard state) player lastMove
  putStrLn "Options: >, <, flip, quit"
  choice <- getLine
  case choice of
    ">"    -> 
      if n + 1 >= length moveList
        then putStrLn "Invalid Move" >> stepLoop moveList n (caps, state, player, lastMove)
        else case makeMove (moveList !! n) state of
          (newP, newState, newMove) ->
            stepLoop moveList (n + 1) (addCapture newP caps player, newState, player, newMove)
    "<"    -> 
      if n - 1 < 0
        then putStrLn "Invalid Move" >> stepLoop moveList n (caps, state, player, lastMove)
        else case makeMove (reverseMove (moveList !! (n - 1))) state of 
          (_, newState, (x, y)) -> do
            let (newCaps, p) = removeCapture caps ChessWhite -- one list is sufficient here
            stepLoop moveList (n - 1) (newCaps, placePiece p x newState, player, (x, y))
    "flip" -> stepLoop moveList n (caps, state, oppColor player, lastMove)
    "quit" -> main
    _      -> putStrLn "Invalid option" >> stepLoop moveList n (caps, state, player, lastMove)