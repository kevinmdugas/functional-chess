module Validate (validate) where

import Board
import State

validate :: (Maybe ChessMove, Maybe ChessMove) -> ChessColor -> GameState -> Bool
validate (Just standardMove, Nothing) player state = 
  validateStandard standardMove player state
validate (Just kingMove, Just rookMove) player state =
  validateCastle (kingMove, rookMove) player state
validate (_, _) _ _ = False

validateStandard :: ChessMove -> ChessColor -> GameState -> Bool
validateStandard (expectedStart, start, end) player state = do
  let actualStart = getPiece state start
  let actualEnd = getPiece state end
  validStartPos expectedStart actualStart &&
    validEndPos player actualEnd && 
    ( case ptype expectedStart of
        P -> True
        _ -> False )
-- validatePawn

validateCastle :: (ChessMove, ChessMove) -> ChessColor -> GameState -> Bool
validateCastle ((k, ks, ke), (r, rs, re)) player state = do
  let actualK = getPiece state ks
  let actualR = getPiece state rs
  validStartPos k actualK && firstMove actualK &&
    validStartPos r actualR && firstMove actualR &&
    validEndPos player (getPiece state ke) &&
    validEndPos player (getPiece state re)

validEndPos :: ChessColor -> Maybe Piece -> Bool
validEndPos _ Nothing = True
validEndPos c (Just p) = color p == oppColor c

validStartPos :: Piece -> Maybe Piece -> Bool
validStartPos _ Nothing = False
validStartPos expected (Just actual) = expected == actual

firstMove :: Maybe Piece -> Bool
firstMove Nothing = False
firstMove (Just p) = not (moved p)