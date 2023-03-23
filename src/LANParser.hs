module LANParser(parseMove) where

import Board
import Data.Char (ord)

parseMove :: String -> ChessColor -> (Maybe ChessMove, Maybe ChessMove)
parseMove lanStr clr =
  case stripCheck lanStr of
    "O-O"   -> castleKS clr
    "0-0"   -> castleKS clr
    "O-O-O" -> castleQS clr
    "0-0-0" -> castleQS clr
    [file1, rank1, capture, file2, rank2]
      | validSquare file1 rank1 && 
        validCapture capture &&
        validSquare file2 rank2 -> do
      let startPos  = (parseRank rank1, parseFile file1)
          endPos    = (parseRank rank2, parseFile file2)
          pieceType = P
      (return (Piece { color = clr, ptype = pieceType, moved = False }, startPos, endPos), Nothing)
    [p, file1, rank1, capture, file2, rank2]
      | validPiece p && 
        validSquare file1 rank1 && 
        validCapture capture &&
        validSquare file2 rank2 -> do
      let startPos   = (parseRank rank1, parseFile file1)
          endPos     = (parseRank rank2, parseFile file2)
          pieceTypeM = parsePiece p
      case pieceTypeM of
        Just pieceType -> (return (Piece { color = clr, ptype = pieceType, moved = False }, startPos, endPos), Nothing)
        Nothing        -> (Nothing, Nothing)
    _ -> (Nothing, Nothing)

-- Get rid of the check or checkmate character as they are not used
stripCheck :: String -> String
stripCheck [] = []
stripCheck (x:xs)
  | x == '+' || x == '#' = []
  | otherwise = x : stripCheck xs

validPiece :: Char -> Bool
validPiece p = p `elem` "KQRBNP"

validSquare :: Char -> Char -> Bool
validSquare file rank = file `elem` "abcdefgh" && rank `elem` "12345678"

validCapture :: Char -> Bool
validCapture c = c `elem` "-x"

-- Translate file character to 0..7
parseFile :: Char -> Int
parseFile file = ord file - ord 'a'

-- Translate rank character to 0..7
parseRank :: Char -> Int
parseRank rank = 7 - (ord rank - ord '1')

parsePiece :: Char -> Maybe PieceType
parsePiece c = case c of
  'K' -> Just K
  'Q' -> Just Q
  'R' -> Just R
  'B' -> Just B
  'N' -> Just N
  'P' -> Just P
  _   -> Nothing

-- Castle kingside
castleKS :: ChessColor -> (Maybe ChessMove, Maybe ChessMove)
castleKS clr =
  let kingStartPos = if clr == ChessBlack then (0, 4) else (7, 4)
      kingEndPos   = if clr == ChessBlack then (0, 6) else (7, 6)
      rookStartPos = if clr == ChessBlack then (0, 7) else (7, 7)
      rookEndPos   = if clr == ChessBlack then (0, 5) else (7, 5)
      kingMove     = (Piece { color = clr, ptype = K, moved = False }, kingStartPos, kingEndPos)
      rookMove     = (Piece { color = clr, ptype = R, moved = False }, rookStartPos, rookEndPos)
  in (Just kingMove, Just rookMove)

-- Castle queenside
castleQS :: ChessColor -> (Maybe ChessMove, Maybe ChessMove)
castleQS clr =
  let kingStartPos = if clr == ChessBlack then (0, 4) else (7, 4)
      kingEndPos   = if clr == ChessBlack then (0, 2) else (7, 2)
      rookStartPos = if clr == ChessBlack then (0, 0) else (7, 0)
      rookEndPos   = if clr == ChessBlack then (0, 3) else (7, 3)
      kingMove     = (Piece { color = clr, ptype = K, moved = False }, kingStartPos, kingEndPos)
      rookMove     = (Piece { color = clr, ptype = R, moved = False }, rookStartPos, rookEndPos)
  in (Just kingMove, Just rookMove)
