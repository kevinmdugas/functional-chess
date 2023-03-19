module Validate (validate) where

import Board
import State

validate :: (Maybe ChessMove, Maybe ChessMove) -> ChessColor -> GameState -> Bool
validate (Just (p, s, e), Nothing) player state = True
validate (Just (k, ks, ke), Just (r, rs, re)) player state =
  k == getPiece state ks &&
  r == getPiece state rs &&
  validEndSquare player (getPiece state ke) &&
  validEndSquare player (getPiece state re)
validate (_, _) _ _ = False

validEndSquare :: ChessColor -> Maybe Piece -> Bool
validEndSquare _ Nothing = True
validEndSquare c (Just p) = color p == oppColor c