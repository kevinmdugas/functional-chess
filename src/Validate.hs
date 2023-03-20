module Validate (validate) where

import Board
import State

import Data.Maybe

data Direction = H | V | D | DQ1 | DQ2 | DQ3 | DQ4 deriving (Eq)
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
        P -> validatePawn player start end state
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
validatePawn player (sr, sc) end state 
  | (player == ChessBlack && end == (sr+1, sc)) ||
    (player == ChessWhite && end == (sr-1, sc)) = isNothing (getPiece state end)
  | (player == ChessBlack && end == (sr+2, sc)) ||
    (player == ChessWhite && end == (sr-2, sc))  =
      firstMove (getPiece state (sr, sc)) && 
      isNothing (getPiece state (sr+1, sc)) && 
      isNothing (getPiece state (sr+2, sc))
  | (player == ChessBlack && (end == (sr+1, sc+1) || end == (sr+1, sc-1))) ||
    (player == ChessWhite && (end == (sr-1, sc-1) || end == (sr-1, sc+1))) =
      isOppPiece (getPiece state end) player
  | otherwise = False

isOppPiece :: Maybe Piece -> ChessColor -> Bool
isOppPiece Nothing _ = False
isOppPiece (Just p) player = color p == oppColor player
  
createVector :: Direction -> Pos -> Pos -> GameState -> Vector
createVector dir (sr, sc) (er, ec) state = case dir of
  V   -> do
          let (s, e) = getHVBounds sr er
          [getPiece state (i,sc) | i <- [s..e]]
  H   -> do
          let (s, e) = getHVBounds sc ec
          [getPiece state (sr,j) | j <- [s..e]]
  DQ1 -> do
          let coords = zip [sr+1..er-1] [sc+1..ec-1]
          [getPiece state (i, j) | (i, j) <- coords]
  DQ2 -> do
          let coords = zip [sr+1..er-1] (reverse [ec+1..sc-1])
          [getPiece state (i, j) | (i, j) <- coords]
  DQ3 -> do
          let coords = zip [er+1..sr-1] [ec+1..sc-1]
          [getPiece state (i, j) | (i, j) <- coords]
  DQ4 -> do
          let coords = zip (reverse [er+1..sr-1]) [sc+1..ec-1]
          [getPiece state (i, j) | (i, j) <- coords]

getHVBounds :: Int -> Int -> Pos
getHVBounds s e | s > e     = (e, s-1)
                | otherwise = (s+1, e)

isClearPath :: Vector -> Bool
isClearPath [] = True
isClearPath vs = foldr ((&&) . isNothing) True vs

validEndPos :: ChessColor -> Maybe Piece -> Bool
validEndPos _ Nothing = True
validEndPos c (Just p) = color p == oppColor c

validStartPos :: Piece -> Maybe Piece -> Bool
validStartPos _ Nothing = False
validStartPos expected (Just actual) = expected == actual

firstMove :: Maybe Piece -> Bool
firstMove Nothing = False
firstMove (Just p) = not (moved p)