module Validate (validate) where

import Board
import State

validate :: (Maybe ChessMove, Maybe ChessMove) -> ChessColor -> GameState -> Bool
validate (Just (p, s, e), Nothing) player state = True
validate (Just kingMove, Just rookMove) player state =
  validateCastle (kingMove, rookMove) player state
validate (_, _) _ _ = False

validateCastle :: (ChessMove, ChessMove) -> ChessColor -> GameState -> Bool
validateCastle ((k, ks, ke), (r, rs, re)) player state =
  validFirstMove k (getPiece state ks) &&
  validFirstMove r (getPiece state rs) &&
  validEndSquare player (getPiece state ke) &&
  validEndSquare player (getPiece state re)

validFirstMove :: Maybe Piece -> Maybe Piece -> Bool
validFirstMove Nothing Nothing = True
validFirstMove _ Nothing = False
validFirstMove Nothing _ = False
validFirstMove (Just expected) (Just actual) = 
  expected == actual && not (moved actual)

validEndSquare :: ChessColor -> Maybe Piece -> Bool
validEndSquare _ Nothing = True
validEndSquare c (Just p) = color p == oppColor c