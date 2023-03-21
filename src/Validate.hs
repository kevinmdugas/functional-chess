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
        R -> validateHorVert start end state
        B -> validateDiag start end state
        Q -> validateDiag start end state || validateHorVert start end state
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

validateHorVert :: Pos -> Pos -> GameState -> Bool
validateHorVert (sr, sc) (er, ec) state 
  | sr == er = isClearPath $ createVector (Just H) (sr, sc) (er, ec) state
  | sc == ec = isClearPath $ createVector (Just V) (sr, sc) (er, ec) state
  | otherwise = False

validateDiag :: Pos -> Pos -> GameState -> Bool
validateDiag start end state = do
  let quadrant = getQuadrant start end
  isClearPath $ createVector quadrant start end state

getQuadrant :: Pos -> Pos -> Maybe Direction
getQuadrant (sr, sc) end = case end of
  (er, ec) | er < sr && ec > sc -> Just DQ1
  (er, ec) | er < sr && ec < sc -> Just DQ2
  (er, ec) | er > sr && ec < sc -> Just DQ3
  (er, ec) | er > sr && ec > sc -> Just DQ4
  (_, _) -> Nothing

isOppPiece :: Maybe Piece -> ChessColor -> Bool
isOppPiece Nothing _ = False
isOppPiece (Just p) player = color p == oppColor player
  
createVector :: Maybe Direction -> Pos -> Pos -> GameState -> Maybe Vector
createVector dir (sr, sc) (er, ec) state = case dir of
  Just V   -> do
                let (s, e) = getHVBounds sr er
                Just [getPiece state (i,sc) | i <- [s..e]]
  Just H   -> do
                let (s, e) = getHVBounds sc ec
                Just [getPiece state (sr,j) | j <- [s..e]]
  Just DQ2 -> do
                if inPath (er, ec) (zip (reverse [er..sr-1]) (reverse [ec..sc-1])) then do
                  let coords = zip [er+1..sr-1] [ec+1..sc-1]
                  Just [getPiece state (i, j) | (i, j) <- coords]
                else Nothing
  Just DQ3 -> do
                if inPath (er, ec) (zip [sr+1..er] (reverse [ec..sc-1])) then do
                  let coords = zip [sr+1..er-1] (reverse [ec+1..sc-1])
                  Just [getPiece state (i, j) | (i, j) <- coords]
                else Nothing
  Just DQ1 -> do
                if inPath (er,ec) (zip (reverse [er..sr-1]) [sc+1..ec]) then do
                  let coords = zip (reverse [er+1..sr-1]) [sc+1..ec-1]
                  Just [getPiece state (i, j) | (i, j) <- coords]
                else Nothing
  Just DQ4 -> do
                if inPath (er, ec) (zip [sr+1..er] [sc+1..ec]) then do
                  let coords = zip [sr+1..er-1] [sc+1..ec-1]
                  Just [getPiece state (i, j) | (i, j) <- coords]
                else Nothing
  Nothing  -> Nothing

inPath :: Pos -> [Pos] -> Bool 
inPath _ [] = False
inPath end (x:xs) | end == x    = True
                  | otherwise   = inPath end xs

getHVBounds :: Int -> Int -> Pos
getHVBounds s e | s > e     = (e+1, s-1)
                | otherwise = (s+1, e-1)

isClearPath :: Maybe Vector -> Bool
isClearPath Nothing   = False
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
