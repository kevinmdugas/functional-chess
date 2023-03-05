module Piece where

data ChessColor = ChessBlack | ChessWhite deriving (Eq, Show)
data PieceType = P | N | B | R | Q | K deriving (Show, Read, Eq, Ord)
data Piece = Piece { color :: ChessColor, ptype :: PieceType } deriving Show
instance Eq Piece where
  (Piece c1 t1) == (Piece c2 t2) = c1 == c2 && t1 == t2