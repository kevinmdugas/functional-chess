module Interface (
  menu,
  display,
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

printBoard :: Board -> IO ()
printBoard board = do
  putStrLn " +------------------------+"
  mapM_ printRow (zip board [8,7..1])
  putStrLn " +------------------------+"
  putStrLn "   A  B  C  D  E  F  G  H  "

printRow :: (Pos, Pos) -> ([Square], Int) -> IO ()
printRow lastMove (row, num) = do
  putStr (show num ++ "|")
  mapM_ (printSquare lastMove) row
  putStrLn ("|" ++ show num)

printSquare :: (Pos, Pos) -> Square -> IO ()
printSquare (start, end) (Square piece tileColor index) = 
  if index == start || index == end then
    setSGR [SetColor Background Dull Yellow] >> printPiece piece >> setSGR [Reset]
  else
    case tileColor of
      ChessBlack -> setSGR [SetColor Background Dull Black] 
        >> printPiece piece >> setSGR [Reset]
      ChessWhite -> setSGR [SetColor Background Dull White] 
        >> printPiece piece >> setSGR [Reset]

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
