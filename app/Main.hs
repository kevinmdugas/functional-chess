module Main (main) where

import qualified Text.Read as Text
  
import Board
import State
import Interface
import LANParser

main :: IO ()
main = do
  mapM_ print menu
  choice <- getLine
  if choice == "1" then play (Nothing, startState, ChessWhite)
  else if choice == "2" then review
  else if choice == "3" then return ()
  else do
    putStrLn "Invalid input"
    main

play :: (Maybe Piece, GameState, ChessColor) -> IO ()
play (p, state, player) = do
  let nextPlayer = case player of
                     ChessWhite -> ChessBlack
                     ChessBlack -> ChessWhite
  printBoard (updateBoard emptyBoard state) player
  putStrLn "Enter move (using long algebraic notation):"
  moveStr <- getLine
  let moveSet = parseMove moveStr player
  putStrLn (show moveSet)
  case moveSet of
    (Just (piece1, start1, end1), Just (piece2, start2, end2)) -> do
      putStrLn "Castle move" -- castle
      let (newPiece1, newState1) = apply move (start1, end1) state
      let (newPiece2, newState2) = apply move (start2, end2) newState1
      play (p, newState2, nextPlayer)
    (Just (piece, start, end), Nothing) -> do
      putStrLn "Valid input"
      let (newPiece, newState) = apply move (start, end) state
      play (newPiece, newState, nextPlayer)
    (Nothing, Nothing) -> do
      putStrLn "Invalid input" -- print error
      play (p, state, player)

getPositions :: String -> String -> (Pos, Pos)
getPositions cur dest = (Text.read cur :: Pos, Text.read dest :: Pos)

review :: IO ()
review = undefined