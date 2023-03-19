module Board(
  ChessColor (..),
  oppColor,
  PieceType (..),
  Piece (..),
  Pos,
  Square (..),
  Board,
  GameState,
  ChessMove,
  emptyBoard,
  updateBoard
) where

data ChessColor = ChessBlack | ChessWhite deriving Eq
instance Show ChessColor where
  show ChessBlack = "Black"
  show ChessWhite = "White"
oppColor :: ChessColor -> ChessColor
oppColor ChessWhite = ChessBlack
oppColor ChessBlack = ChessWhite

data PieceType = P | N | B | R | Q | K deriving (Show, Read, Eq, Ord)

data Piece = Piece {
  color :: ChessColor,
  ptype :: PieceType,
  moved :: Bool
} deriving Show
instance Eq Piece where
  (Piece c1 t1 m1) == (Piece c2 t2 m2) = c1 == c2 && t1 == t2 && m1 == m2

type Pos = (Int, Int)

data Square = Square { 
  piece :: Maybe Piece,
  tile  :: ChessColor,
  index :: Pos
} deriving (Show, Eq)

type Board = [[Square]]

type GameState = [[Maybe Piece]]

type ChessMove = (Maybe Piece, Pos, Pos)

emptyBoard :: Board
emptyBoard = [[Square Nothing 
  (if even (i + j) then ChessWhite else ChessBlack) (j,i) | i <- [0..7]] | j <- [0..7]]

updateBoard :: Board -> GameState -> Board
updateBoard = zipWith (zipWith updateSquare)

updateSquare :: Square -> Maybe Piece -> Square
updateSquare square maybePiece = square { piece = maybePiece }
