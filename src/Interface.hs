module Interface (
  menu,
  reviewMenu,
  display,
) where

import Board
import System.Console.ANSI

menu :: [String]
menu = 
  [ "+------------------------------+",
    "|       FUNCTIONAL CHESS       |",
    "|------------------------------|",
    "|                              |",
    "| 1. Play (Human vs Human)     |",
    "| 2. Load game for review      |",
    "| 3. Quit                      |",
    "|                              |",
    "+------------------------------+" ]

reviewMenu :: [String]
reviewMenu = 
  [ "+------------------------------+",
    "|         Game Review          |",
    "|------------------------------|",
    "| Game review allows you to    |",
    "| step through recorded games. |",
    "|                              |",
    "| Enter the path to a recorded |",
    "| game file to start a review. |",
    "|                              |",
    "| Enter the '>' character to   |",
    "| go forward a move or '<' to  |",
    "| go back a move.              |",
    "|                              |",
    "| Enter \"start\" to go to the   |",
    "| beginning of the game, or    |",
    "| \"quit\" to return to the      |",
    "| main menu                    |",
    "+------------------------------+" ]

display :: Board -> ChessColor -> (Pos, Pos) -> IO ()
display board player lastMove = do
  clearScreen
  putStrLn $ "\t\t" ++ (show player) ++ "'s Turn"
  infoBar $ oppColor player
  printBoard board player lastMove
  infoBar player

infoBar :: ChessColor -> IO ()
infoBar player = putStrLn $ "\t   " ++ (show player) ++ "\t    " ++ "~pieces captured~"

printBoard :: Board -> ChessColor -> (Pos, Pos) -> IO ()
printBoard board player lastMove = do
  if player == ChessWhite then 
    putStrLn      "   a  b  c  d  e  f  g  h   "
      >> putStrLn " +------------------------+ "
      >> mapM_ (printRow lastMove) (zip board [8,7..1])
      >> putStrLn " +------------------------+ "
      >> putStrLn "   a  b  c  d  e  f  g  h   "
  else 
    putStrLn      "   a  b  c  d  e  f  g  h   "
      >> putStrLn " +------------------------+ "
      >> mapM_ (printRow lastMove) (zip ((reverse . map reverse) board) [8,7..1])
      >> putStrLn " +------------------------+ "
      >> putStrLn "   h  g  f  e  d  c  b  a   "

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
