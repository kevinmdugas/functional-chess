module Interface (
  menu,
  printBoard,
) where

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

-- display :: Board -> ChessColor -> IO ()
-- display board player = do
--   infoBar player
--   printBoard board player
--   infoBar !player

-- infoBar :: ChessColor -> IO ()
-- infoBar player = putStr player

printBoard :: Board -> ChessColor -> IO ()
printBoard board player = do
  putStrLn " +------------------------+"
  if player == ChessWhite 
    then mapM_ printRow (zip board [8,7..1])
         >> putStrLn " +------------------------+"
         >> putStrLn "   A  B  C  D  E  F  G  H  "
  else mapM_ printRow (zip ((reverse . map reverse) board) [1,2..8])
       >> putStrLn " +------------------------+"
       >> putStrLn "   H  G  F  E  D  C  B  A  "

printRow :: ([Square], Int) -> IO ()
printRow (row, num) = do
  putStr (show num ++ "|")
  mapM_ printSquare row
  putStrLn ("|" ++ show num)

-- Highlight the start and end square from the last move in yellow
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
