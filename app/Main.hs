module Main (main) where

import qualified Text.Read as Text
  
import Board
import State
import Interface
import LANParser
import GameParser

main :: IO ()
main = do
  mapM_ putStrLn menu
  choice <- getLine
  if choice == "1" then play (Nothing, startState, ChessWhite, ((8,8),(8,8)))
  else if choice == "2" then review
  else if choice == "3" then return ()
  else do
    putStrLn "Invalid input"
    main

play :: (Maybe Piece, GameState, ChessColor, (Pos, Pos)) -> IO ()
play (p, state, player, lastMove) = do
  display (updateBoard emptyBoard state) player lastMove
  putStrLn "Enter move (using long algebraic notation):"
  moveStr <- getLine
  let nextPlayer = oppColor player
  let moveSet = parseMove moveStr player
  -- putStrLn (show moveSet)
  case makeMove moveSet state of
    Nothing -> putStrLn "Invalid Move" >> play (p, state, player, lastMove)
    Just (newP, newState, newMove) -> play (newP, newState, nextPlayer, newMove)

review :: IO ()
review = do
  mapM_ putStrLn reviewMenu
  putStrLn "Enter path to game file: "
  path <- getLine
  moves <- parseGameFile path
  stepLoop moves 0 (startState, ChessWhite, ((8,8),(8,8)))

stepLoop :: [(Maybe ChessMove, Maybe ChessMove)] -> Int -> (GameState, ChessColor, (Pos, Pos)) -> IO ()
stepLoop moveList n (state, player, lastMove) = do
  display (updateBoard emptyBoard state) player lastMove
  putStrLn "Options: >, <, flip, quit"
  choice <- getLine
  case choice of
    ">"     -> case makeMove (moveList !! n) state of
                 Nothing -> putStrLn "Invalid Move" >> stepLoop moveList n (state, player, lastMove)
                 Just (_, newState, newMove) -> stepLoop moveList (n + 1) (newState, player, newMove)
    -- "<"     -> undoMove
    "flip"  -> stepLoop moveList n (state, oppColor player, lastMove)
    "quit"  -> main
    _       -> putStrLn "Invalid option" 
               >> stepLoop moveList n (state, player, lastMove)
  -- stepLoop moveList

makeMove :: (Maybe ChessMove, Maybe ChessMove) -> GameState -> Maybe (Maybe Piece, GameState, (Pos, Pos))
makeMove x state = case x of
  (Just (p1, start1, end1), Just (p2, start2, end2)) -> do -- Castling
    let (newP1, newState1) = apply move (start1, end1) state
    let (newP2, newState2) = apply move (start2, end2) newState1
    return (Nothing, newState2, (start1, end1))
  (Just (p, start, end), Nothing) -> do -- Valid Input
    let (newP, newState) = apply move (start, end) state
    return (newP, newState, (start, end))
  (Nothing, Nothing) -> do -- Invalid Input
    Nothing