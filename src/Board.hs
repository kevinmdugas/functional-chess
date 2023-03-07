module Board where

import Piece

type Pos = (Int, Int)

data Square = Square { 
  pos   :: Pos,
  piece :: Maybe Piece,
  tile  :: ChessColor
} deriving (Show, Eq)

type Board = [[Square]]

startState :: Board
startState = [[
  Square (i, j) (determinePiece i j)
  (if even (i + j) then ChessWhite else ChessBlack) | i <- [0..7]] | j <- [0..7]]

determinePiece :: Int -> Int -> Maybe Piece
determinePiece i j = case (i, j) of
  (c, 0) | c == 0 || c == 7 -> Just (Piece ChessBlack R)
  (c, 0) | c == 1 || c == 6 -> Just (Piece ChessBlack N)
  (c, 0) | c == 2 || c == 5 -> Just (Piece ChessBlack B)
  (3, 0)                    -> Just (Piece ChessBlack Q)
  (4, 0)                    -> Just (Piece ChessBlack K)
  (_, 1)                    -> Just (Piece ChessBlack P)
  (_, 6)                    -> Just (Piece ChessWhite P)
  (c, 7) | c == 0 || c == 7 -> Just (Piece ChessWhite R)
  (c, 7) | c == 1 || c == 6 -> Just (Piece ChessWhite N)
  (c, 7) | c == 2 || c == 5 -> Just (Piece ChessWhite B)
  (3, 7)                    -> Just (Piece ChessWhite Q)
  (4, 7)                    -> Just (Piece ChessWhite K)
  (_, _)                    -> Nothing

-- newtype ST = S { apply :: ( Pos, Maybe Piece ) -> Board -> (Maybe Piece, Board) }

-- getPiece :: Board -> Pos -> Maybe Piece
-- getPiece board (x, y) =  piece (board !! x !! y )

-- move :: ST
-- move = S $ \((x,y), p) board -> (
--     getPiece board (x,y),
--     updateBoard (x,y) p board
--   )

-- updateBoard :: Pos -> Maybe Piece -> Board -> Board
-- updateBoard (x,y) p board = [[
--   Square (i,j) (if (i,j) == (x,y) then p else getPiece board (i,j)) (if even (i + j) then ChessWhite else ChessBlack) | i <- [0..7]] | j <- [0..7]]

-- updateSquare :: Square -> Maybe Piece -> Square
-- updateSquare square maybePiece = square { piece = maybePiece }