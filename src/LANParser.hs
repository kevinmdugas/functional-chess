module LANParser where

import Piece
import Data.Char (ord)

type Pos = (Int, Int)
type ChessMove = (Maybe Piece, Pos, Pos)

parseMove :: String -> ChessColor -> Maybe ChessMove
parseMove lanStr clr =
  case lanStr of
    -- "O-O"   -> castle clr
    -- "O-O-O" -> castle clr
    -- "0-0"   -> castle clr
    -- "0-0-0" -> castle clr
    [file1, rank1, _, file2, rank2] | validSquare file1 rank1 && 
                                      validSquare file2 rank2 -> do
      let startPos  = (parseFile file1, parseRank rank1)
      let endPos    = (parseFile file2, parseRank rank2)
      let pieceType = P
      return (Just Piece { color = clr, ptype = pieceType }, startPos, endPos)
    [piece, file1, rank1, _, file2, rank2] | validPiece piece && 
                                             validSquare file1 rank1 && 
                                             validSquare file2 rank2 -> do
      let startPos  = (parseFile file1, parseRank rank1)
      let endPos    = (parseFile file2, parseRank rank2)
      let pieceTypeM = parsePiece piece
      case pieceTypeM of
        Just pieceType -> return (Just Piece { color = clr, ptype = pieceType }, startPos, endPos)
        Nothing        -> Nothing
    _ -> Nothing

validSquare :: Char -> Char -> Bool
validSquare file rank = file `elem` "abcdefgh" && rank `elem` "12345678"

validPiece :: Char -> Bool
validPiece p = p `elem` "KQRBNP"

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
