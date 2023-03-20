module Validate (validate) where

import Board
import State

import Data.Maybe

data Direction = H | V | D deriving (Eq)
type Vector = [Maybe Piece]

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

validateCastle :: (ChessMove, ChessMove) -> ChessColor -> GameState -> Bool
validateCastle ((k, ks, ke), (r, rs, re)) player state = do
  let actualK = getPiece state ks
  let actualR = getPiece state rs
  validStartPos k actualK && firstMove actualK &&
    validStartPos r actualR && firstMove actualR &&
    validEndPos player (getPiece state ke) &&
    validEndPos player (getPiece state re)

validatePawn :: ChessColor -> Pos -> Pos -> GameState -> Bool
validatePawn player (sr, sc) end state = case end of
  (sr+1, sc) -> isNothing (getPiece state end)
  (sr+2, sc) -> firstMove startPiece
  (sr+1, sc+1) ->
  (sr+1. sc-1) ->
  (_, _) -> False

createVector :: Direction -> Pos -> Pos -> GameState -> Maybe Vector
createVector dir (sr, sc) (er, ec) state = case dir of
  V -> if sc /= ec then Nothing else do
        let (s, e) = getVerticalBounds sr er
        Just [getPiece state (i,sc) | i <- [s..e]]

getVerticalBounds :: Int -> Int -> Pos
getVerticalBounds sr er | sr+1 > er = (er, sr+1)
                        | otherwise = (sr+1, er)

isClearPath :: Maybe Vector -> Bool
isClearPath Nothing = False
isClearPath (Just []) = True
isClearPath (Just vs) = foldr ((&&) . isNothing) True vs

validEndPos :: ChessColor -> Maybe Piece -> Bool
validEndPos _ Nothing = True
validEndPos c (Just p) = color p == oppColor c

validStartPos :: Piece -> Maybe Piece -> Bool
validStartPos _ Nothing = False
validStartPos expected (Just actual) = expected == actual

firstMove :: Maybe Piece -> Bool
firstMove Nothing = False
firstMove (Just p) = not (moved p)