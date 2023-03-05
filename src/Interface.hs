module Interface where

import System.Console.ANSI

menu :: [String]
menu = [ "+--------------------------+",
         "|     FUNCTIONAL CHESS     |",
         "|--------------------------|",
         "|                          |",
         "| 1. Play (Human vs Human) |",
         "| 2. Load game for review  |",
         "|                          |",
         "+--------------------------+" ]


play :: IO ()
play = do
  printBoard $ updateBoard emptyBoard startState
  attemptedMove <- getLine
  putStrLn attemptedMove

review :: IO ()
review = undefined

type Pos = (Int, Int)
data ChessColor = ChessBlack | ChessWhite deriving (Eq, Show)
data PieceType = P | N | B | R | Q | K deriving (Show, Read, Eq, Ord)
data Piece = Piece { color :: ChessColor, ptype :: PieceType } deriving Show
instance Eq Piece where
  (Piece c1 t1) == (Piece c2 t2) = c1 == c2 && t1 == t2

data Square = Square { piece :: Maybe Piece, tile :: ChessColor } deriving (Show, Eq)
type Board = [[Square]]

emptyBoard :: Board
emptyBoard = [[Square Nothing (if (i + j) `mod` 2 == 0 then ChessWhite else ChessBlack) | i <- [0..7]] | j <- [0..7]]

updateBoard :: Board -> [[Maybe Piece]] -> Board
updateBoard board boardState = zipWith (zipWith updateSquare) board boardState

updateSquare :: Square -> Maybe Piece -> Square
updateSquare square maybePiece = square { piece = maybePiece }

startState :: [[Maybe Piece]]
startState = [
  [ Just (Piece ChessBlack R), Just (Piece ChessBlack N), 
    Just (Piece ChessBlack B), Just (Piece ChessBlack Q), 
    Just (Piece ChessBlack K), Just (Piece ChessBlack B), 
    Just (Piece ChessBlack N), Just (Piece ChessBlack R) ], 
    replicate 8 (Just (Piece ChessBlack P)),
    replicate 8 (Nothing),
    replicate 8 (Nothing),
    replicate 8 (Nothing),
    replicate 8 (Nothing),
    replicate 8 (Just (Piece ChessWhite P)),
    [ Just (Piece ChessWhite R), Just (Piece ChessWhite N), 
      Just (Piece ChessWhite B), Just (Piece ChessWhite Q), 
      Just (Piece ChessWhite K), Just (Piece ChessWhite B), 
      Just (Piece ChessWhite N), Just (Piece ChessWhite R) ] ]

printBoard :: Board -> IO ()
printBoard board = do
  putStrLn " +------------------------+"
  mapM_ printRow (zip board [8,7..1])
  putStrLn " +------------------------+"
  putStrLn "   A  B  C  D  E  F  G  H  "

printRow :: ([Square], Int) -> IO ()
printRow (row, num) = do
    putStr (show num ++ "|")
    mapM_ printSquare row
    putStrLn ("|" ++ show num)

printSquare :: Square -> IO ()
printSquare (Square piece tileColor) = case tileColor of
  ChessBlack -> setSGR [SetColor Background Vivid Black] >> printPiece piece >> setSGR [Reset]
  ChessWhite -> setSGR [SetColor Background Vivid White] >> printPiece piece >> setSGR [Reset]

printPiece :: Maybe Piece -> IO ()
printPiece Nothing = putStr "   "
printPiece (Just piece) = case color piece of
  ChessWhite -> setSGR [SetColor Foreground Vivid Blue] >> putStr (printSymbol (ptype piece)) >> setSGR [Reset]
  ChessBlack -> setSGR [SetColor Foreground Vivid Red]  >> putStr (printSymbol (ptype piece)) >> setSGR [Reset]

printSymbol :: PieceType -> String
printSymbol ptype = case ptype of
  P -> " P "
  N -> " N "
  B -> " B "
  R -> " R "
  Q -> " Q "
  K -> " K "