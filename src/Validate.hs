module Validate (validate) where

import Board
import State

validate :: (Maybe ChessMove, Maybe ChessMove) -> ChessColor -> GameState -> Bool
validate (Just (expected, start, end), Nothing) player state = do
  let actual = getPiece state start
  validStartPos expected actual && ( case ptype expected of
      P -> True
      _ -> False )
validate (Just kingMove, Just rookMove) player state =
  validateCastle (kingMove, rookMove) player state
validate (_, _) _ _ = False

-- validatePawn

validateCastle :: (ChessMove, ChessMove) -> ChessColor -> GameState -> Bool
validateCastle ((k, ks, ke), (r, rs, re)) player state =
  validFirstMove k (getPiece state ks) &&
  validFirstMove r (getPiece state rs) &&
  validEndPos player (getPiece state ke) &&
  validEndPos player (getPiece state re)

validFirstMove :: Piece -> Maybe Piece -> Bool
validFirstMove _ Nothing = False
validFirstMove expected (Just actual) = 
  expected == actual && not (moved actual)

validEndPos :: ChessColor -> Maybe Piece -> Bool
validEndPos _ Nothing = True
validEndPos c (Just p) = color p == oppColor c

validStartPos :: Piece -> Maybe Piece -> Bool
validStartPos _ Nothing = False
validStartPos expected (Just actual) = expected == actual