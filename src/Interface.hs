module Interface where

import Piece
import Board
import System.Console.ANSI

menu :: [String]
menu = [ "+--------------------------+",
         "|     FUNCTIONAL CHESS     |",
         "|--------------------------|",
         "|                          |",
         "| 1. Play (Human vs Human) |",
         "| 2. Load game for review  |",
         "| 3. Quit                  |",
         "|                          |",
         "+--------------------------+" ]

printBoard :: Board -> IO ()
printBoard board = do
  putStrLn " +------------------------+"
  -- let board' = rotateBoard board
  -- mapM_ printRow (zip (reverse board') [8,7..1])
  mapM_ printRow (zip (reverse board) [8,7..1])
  putStrLn " +------------------------+"
  putStrLn "   A  B  C  D  E  F  G  H  "

printRow :: ([Square], Int) -> IO ()
printRow (row, num) = do
  putStr (show num ++ "|")
  mapM_ printSquare row
  putStrLn ("|" ++ show num)

printSquare :: Square -> IO ()
printSquare (Square piece tileColor) = case tileColor of
  ChessBlack -> setSGR [SetColor Background Dull Black] >> printPiece piece >> setSGR [Reset]
  ChessWhite -> setSGR [SetColor Background Dull White] >> printPiece piece >> setSGR [Reset]

printPiece :: Maybe Piece -> IO ()
printPiece Nothing = putStr "   "
printPiece (Just piece) = case color piece of
  ChessWhite -> setSGR [SetColor Foreground Dull Blue] >> putStr (printSymbol (ptype piece)) >> setSGR [Reset]
  ChessBlack -> setSGR [SetColor Foreground Dull Red]  >> putStr (printSymbol (ptype piece)) >> setSGR [Reset]

printSymbol :: PieceType -> String
printSymbol ptype = case ptype of
  P -> " P "
  N -> " N "
  B -> " B "
  R -> " R "
  Q -> " Q "
  K -> " K "

-- rotateBoard :: Board -> Board
-- rotateBoard board = [[
--   Square (getPiece board (j,i)) (getTile board (i,j))
--     | i <- [0..length (head board) - 1]]
--     | j <- [0..length board - 1]]