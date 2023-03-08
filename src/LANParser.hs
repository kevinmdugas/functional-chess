module LANParser where

import Piece
import Data.Char (ord)
import Control.Monad (guard)

type Pos = (Int, Int)
type ChessMove = (Maybe Piece, Pos, Pos)

parseMove :: String -> ChessColor -> Maybe ChessMove
parseMove lanStr clr =
  case lanStr of
    -- "O-O"   -> castle
    -- "O-O-O" -> castle
    -- "0-0"   -> castle
    -- "0-0-0" -> castle
    [file1, rank1, _, file2, rank2] -> do
      let startPos  = (parseFile file1, parseRank rank1)
      let endPos    = (parseFile file2, parseRank rank2)
      let pieceType = P
      return (Just Piece { color = clr, ptype = pieceType }, startPos, endPos)
    [piece, file1, rank1, _, file2, rank2] -> do
      guard (piece `elem` "KQRBNP")
      let startPos  = (parseFile file1, parseRank rank1)
      let endPos    = (parseFile file2, parseRank rank2)
      let pieceTypeM = parsePiece piece
      case pieceTypeM of
        Just pieceType -> return (Just Piece { color = clr, ptype = pieceType }, startPos, endPos)
        Nothing        -> Nothing
    _ -> Nothing

parseFile :: Char -> Int
parseFile file = ord file - ord 'a'

parseRank :: Char -> Int
parseRank rank = ord rank - ord '1'

parsePiece :: Char -> Maybe PieceType
parsePiece c = case c of
  'K' -> Just K
  'Q' -> Just Q
  'R' -> Just R
  'B' -> Just B
  'N' -> Just N
  'P' -> Just P
  _   -> Nothing

-- parsePosition :: Parser Position
-- parsePosition = do
--   file <- parseFile <$> many1 letter
--   rank <- parseRank <$> many1 digit
--   return (file, rank)
