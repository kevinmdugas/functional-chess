module Interface (
  menu,
  reviewMenu,
  display,
  printBoard,
  winScreen,
  reviewResult
) where

import Board
import System.Console.ANSI

menu :: [String]
menu = 
  [ "+-------------------------------+",
    "|       FUNCTIONAL CHESS        |",
    "|-------------------------------|",
    "|                               |",
    "| 1. Play (Human vs Human)      |",
    "| 2. Load game for review       |",
    "| 3. Quit                       |",
    "|                               |",
    "+-------------------------------+" ]

reviewMenu :: [String]
reviewMenu = 
  [ "+-------------------------------+",
    "|          Game Review          |",
    "|-------------------------------|",
    "| Game review allows you to     |",
    "| step through recorded games.  |",
    "|                               |",
    "| Enter the path to a recorded  |",
    "| game file to start a review.  |",
    "|                               |",
    "| Enter the '>' character to go |",
    "| forward a move or '<' to go   |",
    "| back a move.                  |",
    "|                               |",
    "| Enter \"flip\" to see the board |",
    "| from the other player's       |",
    "| perspective, or \"quit\" to     |",
    "| return to the main menu.      |",
    "+-------------------------------+" ]

winScreen :: ChessColor -> [String]
winScreen c =
  [ "+-------------------------------+",
    "|          " ++ show c ++ " Wins!          |",
    "+-------------------------------+" ]

reviewResult :: String -> String
reviewResult "1-0"     = "End of Game Reached\t    Result: White Won"
reviewResult "1/2-1/2" = "End of Game Reached\t    Result: Draw"
reviewResult "0-1"     = "End of Game Reached\t    Result: Black Won"
reviewResult _         = "End of Game Reached\t    Result: Error"

-- Display the board between additional info for each player
display :: Board -> Captures -> ChessColor -> (Pos, Pos) -> IO ()
display board (whiteCaps, blackCaps) player lastMove = do
  putStrLn $ "\t" ++ show player ++ "'s Turn"
  infoBar (oppColor player) (if oppColor player == ChessWhite then whiteCaps else blackCaps)
  printBoard board player lastMove
  infoBar player (if player == ChessWhite then whiteCaps else blackCaps)

-- Display player and their pieces captured
infoBar :: ChessColor -> [Maybe Piece] -> IO ()
infoBar player ps = 
  putStrLn $ "\t   " ++ show player ++ "\t    " ++ printCaptures ps

printCaptures :: [Maybe Piece] -> String
printCaptures []            = ""
printCaptures ((Just p):ps) = show (ptype p) ++ printCaptures ps
printCaptures (Nothing:ps)  = printCaptures ps

-- Print the chessboard from the white or black perspective
printBoard :: Board -> ChessColor -> (Pos, Pos) -> IO ()
printBoard board player lastMove = do
  if player == ChessWhite then 
    putStrLn      "   a  b  c  d  e  f  g  h   "
      >> putStrLn " +------------------------+ "
      >> mapM_ (printRow lastMove) (zip board [8,7..1])
      >> putStrLn " +------------------------+ "
      >> putStrLn "   a  b  c  d  e  f  g  h   "
  else 
    putStrLn      "   h  g  f  e  d  c  b  a   "
      >> putStrLn " +------------------------+ "
      >> mapM_ (printRow lastMove) (zip ((reverse . map reverse) board) [1,2..8])
      >> putStrLn " +------------------------+ "
      >> putStrLn "   h  g  f  e  d  c  b  a   "

printRow :: (Pos, Pos) -> ([Square], Int) -> IO ()
printRow lastMove (row, num) = do
  putStr (show num ++ "|")
  mapM_ (printSquare lastMove) row
  putStrLn ("|" ++ show num)

printSquare :: (Pos, Pos) -> Square -> IO ()
printSquare (start, end) (Square p tileColor i) = 
  if i == start || i == end then
    setSGR [SetColor Background Dull Yellow] >> printPiece p >> setSGR [Reset]
  else
    case tileColor of
      ChessBlack -> setSGR [SetColor Background Dull Black] 
        >> printPiece p >> setSGR [Reset]
      ChessWhite -> setSGR [SetColor Background Dull White] 
        >> printPiece p >> setSGR [Reset]

printPiece :: Maybe Piece -> IO ()
printPiece Nothing = putStr "   "
printPiece (Just p) = case color p of
  ChessWhite -> 
    setSGR [SetColor Foreground Dull Blue] >> 
    putStr (" " ++ show (ptype p) ++ " ") >> 
    setSGR [Reset]
  ChessBlack -> 
    setSGR [SetColor Foreground Dull Red] >> 
    putStr (" " ++ show (ptype p) ++ " ") >> 
    setSGR [Reset]
